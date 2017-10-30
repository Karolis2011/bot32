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
	register(core, self()),
	initNet(),
	case whereis(bot) of
		undefined -> ok;
		Pid -> Pid ! quit, ok
	end,
	case whereis(heartbeat) of
		undefined -> ok;
		HPid -> HPid ! quit, ok
	end.

initNet() ->
	{ok, ConnPid} = gun:open("gateway.discord.gg", 443, #{protocols=>[http], retry=>0, transport_opts=>[{versions, ['tlsv1.1']}]}),
	MRef = monitor(process, ConnPid),
	logging:log(info, ?MODULE, "starting"),
	loop(ConnPid),
	logging:log(info, ?MODULE, "quitting"),
	timer:sleep(1000),
	gun:close(ConnPid).

loop(ConnPid) ->
	case receive
		{gun_up, ConnPid, _} ->
			gun:ws_upgrade(ConnPid, "/?v=6&encoding=etf");
		{gun_ws_upgrade, ConnPid, ok, Headers} ->
			logging:log(info, ?MODULE, "WebSocks were upgraded"), ok;
		{gun_response, ConnPid, _, _, Status, Headers} ->
			logging:log(info, ?MODULE, "Server returned something not expected"), ok;
		{gun_error, ConnPid, StreamRef, Reason} ->
			logging:log(error, ?MODULE, "There was gateway connection error"), error;
		{gun_ws, ConnPid, Frame} ->
			case Frame of
				{binary, BinData} ->
					ETFFrame = binary_to_term(BinData),
					#{s:=LastS} = ETFFrame,
					config:set_value(temp, [bot, last_s], LastS),
					common:gateway_handle(ConnPid, ETFFrame);
				{text, _} -> logging:log(info, ?MODULE, "Got text frame: ~p", [Frame]);
				{close, _, _} -> timer:sleep(2000), initNet(), quit;
				X -> logging:log(error, ?MODULE, "Received unknown frame type: ~p", [X])
			end, ok;
		{sendheartbeat, _} ->
			common:gateway_send(ConnPid, 1, config:require_value(temp, [bot, last_s]));
		{'DOWN', Mref, process, ConnPid, Reason} ->
			logging:log(info, ?MODULE, "Gateway connection died: ~p", [Reason]), error;
		{respond, {typing, ChannelID}} ->
			common:discord_request(post, io_lib:format("/channels/~p/typing", [ChannelID]), {struct, []}, fun (_) -> ok end), ok;
		{respond, {message, ChannelID, Message}} ->
			common:discord_request(post, io_lib:format("/channels/~p/messages", [ChannelID]), {struct, [{"content", lists:flatten(Message)}]}, fun (_) -> ok end), ok;
		{respond, {dm, UserID, Message}} ->
			case config:get_value(temp, [bot, userDM, UserID]) of
				'$none' -> 
					common:discord_request(post, "/users/@me/channels", {struct, [{"recipient_id", UserID}]}, fun ({struct, Params}) ->
						case lists:keyfind("id", 1, Params) of
							{"id", SChannelID} ->
								{ChannelID, _} = string:to_integer(SChannelID),
								config:set_value(temp, [bot, userDM, UserID], ChannelID),
								core ! {respond, {message, ChannelID, Message}}, ok;
							false -> logging:log(error, ?MODULE, "Didn't find channel ID"), error
						end
					end);
				ChannelID -> self() ! {respond, {message, ChannelID, Message}}
			end, ok;
			% logging:log(info, ?MODULE, "Won't send DM to ~p : ~p", [UserID, Message]);
		T when is_atom(T) -> T;
		X -> logging:log(error, ?MODULE, "Gateway received unknown message ~p", [X])
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
