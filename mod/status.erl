-module(status).
-compile(export_all).

-define(Sep, 16#feff).

do_extras(Tokens, Reply, _) ->
	case lists:dropwhile(fun(X) -> re:run(X, "^byond://.*$", [{capture, none}]) /= match end, Tokens) of
		[] -> ok;
		[IP|_] ->
			case re:run(IP, "^byond://([^:]+)(?::([1-9][0-9]*))$", [{capture, all_but_first, list}]) of
				nomatch -> ok;
				{match, [Addr, Port]} ->
					status(Reply, Addr, list_to_integer(Port), none, [$(,IP,$),$ ], true)
			end
	end.

defaultserver(ChannelID, GuildID) ->
	case config:get_value(config, [?MODULE, default, {guild, GuildID}]) of
		'$none' -> 
			case config:get_value(config, [?MODULE, default, {channel, ChannelID}]) of
				'$none' -> config:require_value(config, [?MODULE, default, default]);
				X -> X
			end;
		X -> X
	end.

servers() -> config:require_value(config, [?MODULE, servers]).

canonicalise(ID) ->
	case config:get_value(config, [?MODULE, alias, ID]) of
		'$none' -> ID;
		T -> T
	end.

get_commands() ->
	[
		{"address",  generic(address), user},
		{"status",   generic(status), user},
		{"players",  generic(players), user},
		{"admins",   generic(admins), user},
		{"mode",     generic(mode), user},
		{"manifest", generic(manifest), user},
		{"revision", generic(revision), user}
	].

get_help("address") -> ["Get the address of the specified server." | help()];
get_help("status") -> ["Check the status of the specified server." | help()];
get_help("players") -> ["Get an online player list of the specified server." | help()];
get_help("admins") -> ["Get an online admin list of the specified server." | help()];
get_help("mode") -> ["Get the mode of the specified server." | help()];
get_help("manifest") -> ["Get the manifest of the specified server." | help()];
get_help("revision") -> ["Get the revision of the specified server." | help()];
get_help(_) -> unhandled.

help() ->
	[
		["Valid servers are: ",
		string:join(lists:map(fun({ID, {Addr,Port,_Name}}) ->
				io_lib:format("'~s' (~s:~b)", [ID, Addr, Port])
			end, servers()), "; ")]
	].

generic(Func) ->
	fun(#{origin:=#{id:=UserID}, channel:=Channel=#{id:=ChannelID}, guild:=#{id:=GuildID}, ping:=P, params:=[]}) ->
		case orddict:find(defaultserver(ChannelID, GuildID), servers()) of
			{ok, {Addr,Port,Name}} -> spawn(status, Func, [ChannelID, P, UserID, Addr, Port, defaultserver(ChannelID, GuildID), Name]), ok;
			error -> {respond, {message, ChannelID, [P, "Failed to find default server for this channel!"]}}
		end;
	   (#{origin:=#{id:=UserID}, channel:=Channel=#{id:=ChannelID}, ping:=P, params:=[Address="byond://"++_]}) ->
		case re:run(Address, "^byond://([^:]+)(?::([1-9][0-9]*))$", [{capture, all_but_first, list}]) of
			{match,[IP,Port]} ->
				spawn(status, Func, [ChannelID, P, UserID, IP, list_to_integer(Port), Address, [$(,Address,$),$ ]]);
			_ -> {respond, {message, ChannelID, [P, "Illegal argument!"]}}
		end;
	   (#{origin:=#{id:=UserID}, channel:=Channel=#{id:=ChannelID}, ping:=P,params:=[ServerID]}) ->
		case orddict:find(canonicalise(ServerID), servers()) of
			{ok, {Addr,Port,Name}} -> spawn(status, Func, [ChannelID, P, UserID, Addr, Port, canonicalise(ServerID), Name]), ok;
			error -> {respond, {message, ChannelID, [P, "Illegal argument!"]}}
		end
	end.

address(ChannelID, Ping, _, S, P, _, Name) ->
	core ! {respond, {message, ChannelID, [Ping, Name, io_lib:format("byond://~s:~b", [S, P])]}}.
status(ChannelID, _, _, S, P, ID, Name) ->
	status(ChannelID, S, P, ID, Name, false).
status(ChannelID, S, P, ID, Name, SilenceErrors) ->
	core ! {respond, {typing, ChannelID}},
	case config:get_value(config, [?MODULE, key, list_to_binary(io_lib:format("~s:~p", [S, P]))]) of
		'$none' -> 
			case byond:send(S, P, "status=2") of
				{error, _} when SilenceErrors -> ok;
				{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
				Dict ->
					Display = [{"Players", "players"}, {"Mode", "mode"}, {"Station Time", "stationtime"}, {"Round Duration", "roundduration"}, {"Map", "map"}],
					DispParts = lists:filtermap(fun({Disp,Key}) ->
						case orddict:find(Key, Dict) of
							{ok, V} -> {true, [Disp, ": ", V]};
							error -> false
						end end, Display),
					core ! {respond, {message, ChannelID, [Name, string:join(DispParts, "; ")]}}
			end;
		Key -> status(ChannelID, S, P, ID, Name, false, Key, json)
	end.
status(ChannelID, S, P, ID, Name, SilenceErrors, Key, json) ->
	Req = [{"query", "get_serverstatus"}, {"auth", Key}],
	case byond:send(S, P, json:write({struct, Req}), false) of
		{error, _} when SilenceErrors -> ok;
		{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
		JData ->
			case util:mochi_to_map(json:parse(JData)) of
				#{"statuscode":=200, "data":=Data} ->
					Display = [{"Players", "players"}, {"Mode", "mode"}, {"Station Time", "stationtime"}, {"Round Duration", "roundduration"}],
					DispParts = lists:filtermap(fun({Disp,Key}) ->
						case maps:find(Key, Data) of
							{ok, V} when is_integer(V) -> {true, [Disp, io_lib:format(": ~p",[V])]};
							{ok, V} -> {true, [Disp, ": ", V]};
							error -> false
						end end, Display),
					core ! {respond, {message, ChannelID, [Name, string:join(DispParts, "; ")]}};
				#{"statuscode":=Code, "response":=Response} ->
					core ! {respond, {message, ChannelID, io_lib:format("~sError: (~p) ~s", [Name, Code, Response])}}
			end
	end.

revision(ChannelID, _, _, S, P, ID, Name) ->
	core ! {respond, {typing, ChannelID}},
	case byond:send(S, P, "revision") of
		{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
		[{"unknown","?"}] -> core ! {respond, {message, ChannelID, io_lib:format("~sRevision unknown.", [Name])}};
		Dict ->
			Branch = safeget(Dict, "branch"),
			Date = safeget(Dict, "date"),
			Rev = safeget(Dict, "revision"),
			GID = safeget(Dict, "gameid"),
			DD = safeget(Dict, "dd_version"),
			DM = safeget(Dict, "dm_version"),
			Msg = case config:get_value(config, [?MODULE, github, ID]) of
				'$none' -> io_lib:format("~sRevision: ~s on ~s at ~s. Game ID: ~s. DM: ~s; DD: ~s", [Name, Rev, Branch, Date, GID, DM, DD]);
				URL -> io_lib:format("~sRevision: ~s on ~s at ~s: ~s. Game ID: ~s. DM: ~s; DD: ~s", [Name, lists:sublist(Rev, 8), Branch, Date, [URL,Rev], GID, DM, DD])
			end,
			core ! {respond, {message, ChannelID, Msg}}
	end.

admins(ChannelID, _, _, S, P, _, Name) ->
	core ! {respond, {typing, ChannelID}},
	case config:get_value(config, [?MODULE, key, list_to_binary(io_lib:format("~s:~p", [S, P]))]) of
		'$none' -> 
			case byond:send(S, P, "status=2") of
				{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name,X])}};
				Dict ->
					Msg = case byond:params2dict(safeget(Dict, "adminlist")) of
						[{"?",none}] -> [Name,"No admins online."];
						Admins ->
							BinMins = lists:map(fun({A,B}) -> {re:replace(A, <<32>>, <<160/utf8>>, [{return, binary}, global]),
																re:replace(B, <<32>>, <<160/utf8>>, [{return, binary}, global])} end, Admins),
							AdminStr = util:binary_join(lists:map(fun({<<A/utf8,B/binary>>,C}) -> CA=a(C), <<A/utf8, ?Sep/utf8, B/binary, " is ", CA/binary, " ", C/binary>> end, BinMins), <<"; ">>),
							[io_lib:format("~sAdmins (~b): ", [Name,length(Admins)]), AdminStr]
					end,
					core ! {respond, {message, ChannelID, Msg}}
			end;
		Key -> admins(ChannelID, none, none, S, P, none, Name, Key, json)
	end.
admins(ChannelID, _, _, S, P, _, Name, Key, json) ->
	Req = [{"query", "get_serverstatus"}, {"auth", Key}, {"status", "2"}],
	case byond:send(S, P, json:write({struct, Req}), false) of
		{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name,X])}};
		JData ->
			case util:mochi_to_map(json:parse(JData)) of
				#{"statuscode":=200, "data":=#{"adminlist":=Admins, "admins":=ACount}} ->
					SNice = maps:fold(fun (Name, Title, NList) ->
						lists:append(NList, [lists:flatten([Name, " - _", Title, "_"])])
						end, [], Admins),
					util:groupstrs(fun(T) -> core ! {respond, {message, ChannelID, [io_lib:format("~sAdmins (~b): ", [Name,length(SNice)]), T]}} end, 2000, lists:sort(SNice), ", ");
				#{"statuscode":=Code, "response":=Response} ->
					core ! {respond, {message, ChannelID, io_lib:format("~sError: (~p) ~s", [Name, Code, Response])}}
			end
	end.

a(<<T/utf8, _/binary>>) ->
	case lists:member(T, "AEIOUaeiou") of
		true -> <<"an">>;
		false -> <<"a">>
	end;
a(_) -> <<"a">>.

mode(ChannelID, _, _, S, P, _, Name) ->
	core ! {respond, {typing, ChannelID}},
	case config:get_value(config, [?MODULE, key, list_to_binary(io_lib:format("~s:~p", [S, P]))]) of
		'$none' -> 
			case byond:send(S, P, "status=2") of
				{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name,X])}};
				Dict ->
					Mode = safeget(Dict, "mode"),
					core ! {respond, {message, ChannelID, [Name, "Mode: ", Mode]}}
			end;
		Key -> mode(ChannelID, none, none, S, P, none, Name, Key, json)
	end.
mode(ChannelID, _, _, S, P, _, Name, Key, json) ->
	Req = [{"query", "get_serverstatus"}, {"auth", Key}],
	case byond:send(S, P, json:write({struct, Req}), false) of
		{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name,X])}};
		JData ->
			case util:mochi_to_map(json:parse(JData)) of
				#{"statuscode":=200, "data":=#{"mode":=Mode}} ->
					core ! {respond, {message, ChannelID, [Name, "Mode: ", Mode]}};
				#{"statuscode":=Code, "response":=Response} ->
					core ! {respond, {message, ChannelID, io_lib:format("~sError: (~p) ~s", [Name, Code, Response])}}
			end
	end.

players(ChannelID, _, UserID, S, P, _, Name) ->
	core ! {respond, {typing, ChannelID}},
	case config:get_value(config, [?MODULE, key, list_to_binary(io_lib:format("~s:~p", [S, P]))]) of
		'$none' -> 
			case byond:send(S, P, "status=2") of
				{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
				Dict ->
					case safeget(Dict, "players") of
						"?" -> core ! {respond, {message, ChannelID, [Name, "Error."]}};
						"0" -> core ! {respond, {message, ChannelID, [Name, "No players present."]}};
						_ ->
							Players = byond:params2dict(safeget(Dict, "playerlist")),
							Ordered = lists:sort(lists:map(fun({X,_}) -> re:replace(X, [32], <<160/utf8>>, [{return, binary}, global]) end, Players)),
							Names = lists:map(fun(<<A/utf8, B/binary>>) -> binary_to_list(<<A/utf8, B/binary>>) end, Ordered),
							util:groupstrs(fun(T) -> core ! {respond, {message, ChannelID, [Name, "Players: ", T]}} end, 2000, Names, ", ")
					end
			end;
		Key -> players(ChannelID, none, UserID, S, P, none, Name, Key, json)
	end.
	
players(ChannelID, _, _, S, P, _, Name, Key, json) ->
	Req = [{"query", "get_serverstatus"}, {"auth", Key}, {"status", "2"}],
	case byond:send(S, P, json:write({struct, Req}), false) of
		{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
		JData ->
			case util:mochi_to_map(json:parse(JData)) of
				#{"statuscode":=200, "data":=#{"playerlist":=Data}} ->
					SData = lists:sort(Data),
					util:groupstrs(fun(T) -> core ! {respond, {message, ChannelID, [Name, "Players: ", T]}} end, 2000, SData, ", ");
				#{"statuscode":=Code, "response":=Response} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: (~p) ~s", [Name, Code, Response])}}
			end
	end.

manifest(ChannelID, _, UserID, S, P, ID, Name) ->
	core ! {respond, {typing, ChannelID}},
	case config:get_value(config, [?MODULE, key, list_to_binary(io_lib:format("~s:~p", [S, P]))]) of
		'$none' -> 
			case byond:send(S, P, "manifest") of
				{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
				[] -> core ! {respond, {message, ChannelID, [Name, "Manifest is empty"]}};
				Dict ->
					Message = lists:foldl(fun ({Dept, Players}, PMsg) ->
						[PMsg, "**", Dept, "**:\r\n", lists:foldl(fun ({Char, Title}, PPMsg) ->
							[PPMsg, Char, " - _", Title, "_\r\n"]
						end, "", byond:params2dict(Players)), "\r\n"]
					end, [Name, "Manifest:\r\n"], Dict),
					Lengths = lists:foldl(fun ({Dept, Players}, Last) -> 
						[Last, " ", Dept, ": ", io_lib:format("~p", [lists:foldl(fun (_, PLast) -> 
							PLast + 1 
						end, 0, byond:params2dict(Players))]), ";"] 
					end, "", Dict),
					core ! {respond, {dm, UserID, [Message]}},
					core ! {respond, {message, ChannelID, [Name, "Manifest lengths:", Lengths]}}
			end;
		Key -> manifest(ChannelID, none, UserID, S, P, ID, Name, Key, json)
	end.
manifest(ChannelID, _, UserID, S, P, _, Name, Key, json) ->
	Req = [{"query", "get_manifest"}, {"auth", Key}],
	case byond:send(S, P, json:write({struct, Req}), false) of
		{error, X} -> core ! {respond, {message, ChannelID, io_lib:format("~sError: ~p", [Name, X])}};
		JData ->
			case util:mochi_to_map(json:parse(JData)) of
				#{"statuscode":=200, "data":=Data} ->
					Message = maps:fold(fun (Dept, Players, PMsg) ->
						[PMsg, "**", Dept, "**:\r\n", maps:fold(fun (Char, Title, PPMsg) ->
							[PPMsg, Char, " - _", Title, "_\r\n"]
						end, "", Players), "\r\n"]
					end, [Name, "Manifest:\r\n"], Data),
					Lengths = maps:fold(fun (Dept, Players, Last) -> [Last, " ", Dept, ": ", io_lib:format("~p", [maps:fold(fun (_, _, PLast) -> PLast + 1 end, 0, Players)]), ";"] end, "", Data),
					core ! {respond, {dm, UserID, [Message]}},
					core ! {respond, {message, ChannelID, [Name, "Manifest lengths:", Lengths]}};
				#{"statuscode":=Code, "response":=Response} ->
					core ! {respond, {message, ChannelID, io_lib:format("~sError: (~p) ~s", [Name, Code, Response])}}
			end
	end.

safeget(Dict, Key) ->
	case orddict:find(Key, Dict) of
		{ok, V} -> V;
		error -> "???"
	end.
