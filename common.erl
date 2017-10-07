-module(common).
-compile(export_all).

-include("definitions.hrl").
-define(MAX_LENGTH, 448).

waitfor_gone(Ident) ->
	case whereis(Ident) of
		undefined -> ok;
		_ -> timer:sleep(100), waitfor_gone(Ident)
	end.

debug(What, Msg) -> io:fwrite("[~s] ~s~n", [What, Msg]).
debug(What, Format, List) ->
	io:fwrite("[~s] ~s~n", [What, io_lib:format(Format, List)]).

flatten(L) -> lists:reverse(flatten(L, [])).

flatten([], O) -> O;
flatten([A|B], O) when is_list(A) -> flatten(B, flatten(A, O));
flatten([A|B], O) -> flatten(B, [A | O]);
flatten(A, O) -> [A | O].

proper([A|B]) when not is_list(B) -> [proper(A), proper(B)];
proper([A|B]) -> [proper(A) | proper(B)];
proper(A) -> A.

% Spawn this method to have your code compiled and reloaded.
purge_call(Module, Function, Param) ->
	code:purge(Module),
	compile:file(Module, [report_errors, report_warnings, {outdir, "./bin"}]),
	code:load_file(Module),
	Module:Function(Param).

purge_call_report(Module, Function, Param, Channel) ->
	code:purge(Module),
	Reply = case compile:file(Module, [return, report_errors, report_warnings, {outdir, "./bin"}]) of
		{ok,_} -> "Update complete.";
		{ok,_,[]} -> "Update complete.";
		{ok,_,Warnings} -> io_lib:format("Update complete; ~b warning(s).",[length(Warnings)]);
		error -> "Unable to update.";
		{error,Errors,[]} -> io_lib:format("Update failed; ~b error(s).",[length(Errors)]);
		{error,Errors,Warnings} -> io_lib:format("Update failed; ~b error(s) and ~b warning(s).",[length(Errors), length(Warnings)])
	end,
	code:load_file(Module),
	self() ! {ircfwd, {msg, {Channel, Reply}}},
	Module:Function(Param).

start() -> spawn(?MODULE, start_nospawn, []).
% start(Server, Transport, Port) -> spawn(?MODULE, start_nospawn, [Server, Transport, Port]).

start_nospawn() ->
	spawn(bot, init, []),
	timer:sleep(300),
	core:init().

gateway_handle(ConnPid, #{op:=0, t:=EventName, d:=Data}) ->
	case whereis(bot) of
		undefined -> logging:log(error, ?MODULE, "Bot PID isn't found."), error;
		Pid -> Pid ! {event, EventName, Data}, ok
	end;
gateway_handle(ConnPid, #{op:=11}) -> % Heartbeat ACK, do nothing, TODO: kill connection if no ACK is sent for 2x BeatInterval
	ok;
gateway_handle(ConnPid, #{op:=1, d:=Snum}) -> % Heartbeat payload, have to send back Heartbeat ACK.
	gateway_send(ConnPid, 11, nil);
gateway_handle(ConnPid, #{op:=10, d:=#{heartbeat_interval:=BeatInterval}}) -> % Hello payload, starts Heartbeat and sends Identify, TODO: Implement Resume
	spawn(common, heartbeat_timer, [BeatInterval]),
	gateway_send(ConnPid, 2, #{<<"token">>=>list_to_binary(config:require_value(config, [bot, token])),
		<<"compress">>=>false,
		<<"large_threshold">>=>50,
		<<"properties">>=>#{
			<<"$os">>=><<"linux">>,
			<<"$browser">>=><<"bot32">>,
			<<"$device">>=><<"bot32">>,
			<<"$referrer">>=><<"">>,
      <<"$referring_domain">>=><<"">>
		},
		<<"shard">>=>[0,1]
		});
gateway_handle(ConnPid, Event) -> % Unknown payload received, all payloads should be handled in some way.
		logging:log(error, ?MODULE, "Unhandled payload: ~p", [Event]).

discord_request(get, Url) ->
	{ok, ConnPid} = gun:open("discordapp.com", 443, #{protocols=>[http], retry=>0}),
	StreamRef = gun:get(ConnPid, ["/api/v6", Url], [
		{<<"Authorization">>, ["Bot ", config:require_value(config, [bot, token])]}
	]),
	receive
		{gun_data, ConnPid, StreamRef, fin, Data} ->
			Data;
		{'DOWN', MRef, process, ConnPid, Reason} ->
			error
	after 1000 ->
		err
	end.

gateway_send(ConnPid, OpCode, Data) ->
	gateway_send(ConnPid, #{<<"op">>=>OpCode, <<"d">>=>Data}).

gateway_send(ConnPid, Frame) ->
	logging:log(info, ?MODULE, "Data sent: ~p", [Frame]),
	gun:ws_send(ConnPid, {binary, term_to_binary(Frame)}).

heartbeat_timer(BeatInterval) ->
	case whereis(heartbeat) of
		undefined ->
			register(heartbeat, self());
		Pid -> ok
	end,
	receive
		quit -> ok
	after
		BeatInterval ->
			case whereis(core) of
				undefined -> ok;
				CorePid -> CorePid ! {sendheartbeat, BeatInterval}, ok
			end,
			heartbeat_timer(BeatInterval)
	end.

% Most things bellow are for IRC

% tcp_parse(S, T, M) ->
% 	Tokens = string:tokens(M, " "),
% 	case hd(Tokens) of
% 		"PING" -> tcp_send(S, T, {pong, string:join(tl(Tokens), " ")});
% 		":" ++ RawOrigin ->
% 			Origin = parse_origin(RawOrigin),
% 			case tl(Tokens) of
% 				% Setup & system
% 				["PONG", _Who | _Comment] -> ok; % common:debug("pong", "~p : ~p", [Who, Comment]), ok;
% 				["NICK", Nick] -> {irc, {nick, {Origin, Nick}}};
% 				["QUIT" | Reason] -> {irc, {quit, {Origin, [tl(hd(Reason)) | tl(Reason)]}}};
% 				["MODE" | Params] -> {irc, {mode, {Origin, Params}}};
% 				["KICK", Channel, Nick | Params] ->
% 					{irc, {kick, {Origin, Nick, Channel, [tl(hd(Params)) | tl(Params)]}}};
% 				["TOPIC", Channel | Topic] ->
% 					{irc, {topic, {Origin, Channel, [tl(hd(Topic)) | tl(Topic)]}}};
%
% 				% Channel management
% 				["JOIN", [_|Channel]] -> {irc, {join, {Origin, Channel}}};
% 				["PART", Channel | Msg] -> {irc, {part, {Origin, Channel, Msg}}};
%
% 				% Messaging
% 				["PRIVMSG", Channel, First | Rest] -> parse_privmsg(Origin, Channel, [tl(First) | Rest]);
% 				["NOTICE", Channel, First | Rest] -> parse_notice(Origin, Channel, [tl(First) | Rest]);
%
% 				% Errors
% 				[Cmd | Params] ->
% 					case string:to_integer(Cmd) of
% 						{error, _} -> logging:log(error, "PARSE", "Unknown TCP message received; ~s : ~p", [Cmd, Params]);
% 						{I, []} ->
% 							Parsed = numeric_parse(I),
% 							{irc, {numeric, {Parsed, Params}}};
% 						{_, _} -> logging:log(error, "PARSE", "Unknown TCP message received; ~s : ~s", [Cmd, string:join(Params, " ")])
% 					end
% 			end;
% 		_ -> logging:log(error, "PARSE", "Expected :, saw '~s'.", [M])
% 	end.
%
% parse_privmsg(Origin, Channel, Message) ->
% 	case hd(Message) of
% 		[] -> ok;
% 		_ ->
% 			FirstFirst = hd(hd(Message)),
% 			LastLast = lists:last(lists:last(Message)),
% 			if
% 				FirstFirst == 1 andalso LastLast == 1 ->
% 						StrippedMessage = string:tokens(lists:reverse(tl(lists:reverse(tl(string:join(Message, " "))))), " "),
% 						{irc, {ctcp, parse_ctcp(Origin, Channel, StrippedMessage)}};
% 				true -> {irc, {msg, {Origin, Channel, Message}}}
% 			end
% 	end.
%
% parse_notice(Origin, Channel, [[]]) -> {irc, {notice, {Origin, Channel, [""]}}};
% parse_notice(Origin, Channel, [[]|Msg]) -> parse_notice(Origin, Channel, Msg);
% parse_notice(Origin, Channel, Message) ->
% 	FirstFirst = hd(hd(Message)),
% 	LastLast = lists:last(lists:last(Message)),
% 	if
% 		FirstFirst == 1 andalso LastLast == 1 ->
% 				StrippedMessage = string:tokens(lists:reverse(tl(lists:reverse(tl(string:join(Message, " "))))), " "),
% 				{irc, {ctcp_re, parse_ctcp(Origin, Channel, StrippedMessage)}};
% 		true -> {irc, {notice, {Origin, Channel, Message}}}
% 	end.
%
% tcp_send(S, T, {pass, Pass}) ->                    raw_send(S, T, ["PASS :", Pass]);
% tcp_send(S, T, {user, {User, Mode, Real}} ) ->     raw_send(S, T, ["USER ",User," ",Mode," * :",Real]);
% tcp_send(S, T, {nick, Nick}) ->                    raw_send(S, T, ["NICK ",Nick]);
% tcp_send(S, T, {mode, {Channel, Mode}}) ->         raw_send(S, T, ["MODE ", Channel, 32, Mode]);
% tcp_send(S, T, {quit, Message}) ->                 raw_send(S, T, ["QUIT :",Message]), core!quit;
% tcp_send(S, T, {pong, Params}) ->                  raw_send(S, T, ["PONG ",Params]);
% tcp_send(S, T, {join, Channel}) ->                 raw_send(S, T, ["JOIN ",Channel]);
% tcp_send(S, T, {part, {Channel, Reason}}) ->       raw_send(S, T, ["PART ",Channel," :",Reason]);
% tcp_send(S, T, {kick, {Channel, User, Reason}}) -> raw_send(S, T, ["KICK ",Channel," ",User," :",Reason]);
% tcp_send(S, T, {msg, {Recipient, Message}}) ->     raw_send(S, T, ["PRIVMSG ",Recipient," :",Message]);
% tcp_send(S, T, {ctcp, {version, R, V}}) ->         raw_send(S, T, ["PRIVMSG ",R," :\1VERSION ",V,1]);
% tcp_send(S, T, {ctcp, {action, R, V}}) ->          raw_send(S, T, ["PRIVMSG ",R," :\1ACTION ",V,1]);
% tcp_send(S, T, {ctcp, {unknown, R, V}}) ->         raw_send(S, T, ["PRIVMSG ",R," :\1",V,1]);
% tcp_send(S, T, {notice, {Recipient, Message}}) ->  raw_send(S, T, ["NOTICE ",Recipient," :",Message]);
% tcp_send(S, T, {ctcp_re, {version, R, V}}) ->      raw_send(S, T, ["NOTICE ",R," :\1VERSION ",V,1]);
% tcp_send(S, T, {ctcp_re, {action, R, V}}) ->       raw_send(S, T, ["NOTICE ",R," :\1ACTION ",V,1]);
% tcp_send(S, T, {ctcp_re, {unknown, R, V}}) ->      raw_send(S, T, ["NOTICE ",R," :\1",V,1]);
% tcp_send(_, _, {T, X}) -> logging:log(error, "SEND", "Unknown IRC message type or fomat {~p, ~p}", [T, X]).
%
% parse_origin(O) ->
% 	case re:run(O, "([^!@]+)(!([^!@]+)@([^!@]+))?", [{capture, all_but_first, list}]) of
% 		nomatch -> notuser;
% 		{match, [N, _, U, H]}-> #user{nick=N, username=U, host=H};
% 		{match, [Svr]} -> Svr;
% 		T -> logging:log(error, "PARSE", "Something broke in user_string; ~p", [T]), error
% 	end.

format_time_difference(T) when T <    60 ->  t_quant(T,          "second");
format_time_difference(T) when T <  3600 -> [t_quant(T div   60, "minute"), t_quant2( T rem    60,           "second")];
format_time_difference(T) when T < 86400 -> [t_quant(T div  3600,  "hour"), t_quant2((T rem  3600) div   60, "minute")];
format_time_difference(T)                -> [t_quant(T div 86400,   "day"), t_quant2((T rem 86400) div 3600,   "hour")].

t_quant(1,U) -> ["1 ", U];
t_quant(V,U) -> [integer_to_list(V),32,U,115].

t_quant2(0,_) -> "";
t_quant2(V,U) -> [", ",t_quant(V,U)].
