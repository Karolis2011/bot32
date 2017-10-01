-module(core).
-compile(export_all).

-define(SOCK_OPTIONS, [list, {packet, line}, {active, true}]).
-include("definitions.hrl").

init() ->
	case config:is_started(config) of
		false -> config:start(config);
		true -> ok
	end,
	config:start(data),
	config:start_transient(temp),

	{ok, ConnPid} = gun:open("gateway.discord.gg", 443, #{protocols=>[http], retry=>0}),
	MRef = monitor(process, ConnPid),
	gun:ws_upgrade(ConnPid, "/?v=6&encoding=etf"),
	register(core, self()),
	logging:log(info, ?MODULE, "starting"),
	loop(ConnPid), %Remake everything related to this
	logging:log(info, ?MODULE, "quitting"),
	% Transport:close(Sock),
	% ssl:stop(),
	timer:sleep(1000),
	gun:close(ConnPid),
	case whereis(bot) of
		undefined -> ok;
		Pid -> Pid ! quit, ok
	end,
	case whereis(heartbeat) of
		undefined -> ok;
		HPid -> HPid ! quit, ok
	end.

loop(ConnPid) ->
	case receive
		{gun_ws_upgrade, ConnPid, ok, Headers} ->
			logging:log(info, ?MODULE, "WebSocks were upgraded"), ok;
		{gun_response, ConnPid, _, _, Status, Headers} ->
			logging:log(info, ?MODULE, "Server returned something"), ok;
		{gun_error, ConnPid, StreamRef, Reason} ->
			logging:log(error, ?MODULE, "There was gun error"), error;
		{gun_ws, ConnPid, Frame} ->
			case Frame of
				{binary, BinData} ->
					% logging:log(info, ?MODULE, "Got bin frame: ~p", [BinData]),
					ETFFrame = binary_to_term(BinData),
					% logging:log(info, ?MODULE, "Got etf frame: ~p", [ETFFrame]),
					#{s:=LastS} = ETFFrame,
					config:set_value(temp, [bot, last_s], LastS),
					common:gateway_handle(ConnPid, ETFFrame);
				{text, _} -> logging:log(info, ?MODULE, "Got frame: ~p", [Frame]);
				X -> logging:log(error, ?MODULE, "Received unknown frame type: ~p", [X])
			end, ok;
		{sendheartbeat, _} ->
			common:gateway_send(ConnPid, 1, config:require_value(temp, [bot, last_s]));
		{'DOWN', Mref, process, ConnPid, Reason} ->
			logging:log(info, ?MODULE, "Gun died: ~p", [Reason]), error;
		T when is_atom(T) -> T;
		X -> logging:log(error, ?MODULE, "Received unknown message ~p", [X])
	after
		30 * 1000 ->
			% logging:log(info, ?MODULE, "30 seconds have passed"), ok
			ok
	end of
		quit -> ok;
		error -> error;
		ok -> loop(ConnPid);
		update -> core:loop(ConnPid);
		S ->
			logging:log(error, ?MODULE, "unknown message ~p, continuing", [S]),
			loop(ConnPid)
	end.
