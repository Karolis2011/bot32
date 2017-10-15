-module(command).
-compile(export_all).

-include("definitions.hrl").

handle_event(newMessage, {_, Tokens, User = #{username:=Username}, Channel = #{id:=ChannelId}, Guild}) ->
	case parse_command(Tokens) of
		{RCommand, RArguments, Selector} ->
			logging:log(info, ?MODULE, "Command in ~p from ~s: ~s~s ~s", [ChannelId, Username, RCommand, if Selector /= [] -> [$@|Selector]; true -> [] end, string:join(RArguments, " ")]),
			{Command, Arguments} = lists:foldl(fun(Module, {C,A}) ->
					util:call_or(Module, pre_command, [C, A], {C, A})
				end, {RCommand, RArguments}, config:require_value(config, [bot, modules])),
			Rank = permissions:rankof(User, Channel, Guild),
			case handle_command(Rank, User, Channel, Guild, Command, Arguments, Selector) of
				{respond, Result} ->
					core ! {respond, Result};
				{'EXIT', Term} ->
					logging:log(error, ?MODULE, "handle_command exited ~p", [Term]);
				_ -> ok
			end;
		notcommand ->
			RP = reply_ping(User, Channel),
			lists:foreach(fun(Module) ->
					util:call_or(Module, do_extras, [Tokens, ChannelId, RP], null),
					% util:call_or(Module, handle_event, [msg_nocommand, Params], null)
					ok
				end, config:require_value(config, [bot, modules]))
	end;
handle_event(_, _) -> ok.

parse_command([]) -> notcommand;
parse_command(Params) ->
	if
		length(Params) > 1 ->
			BotAliases = config:require_value(temp, [bot, names]),
			case lists:any(fun(Alias) ->
						R = util:regex_escape(Alias),
						re:run(hd(Params), <<"^", R/binary, "($|[^a-zA-Z0-9])">>, [caseless, {capture, none}]) == match
					end, BotAliases) of
				true ->
					case tl(Params) of
						[] -> {[], [], []};
						_ -> desel(hd(tl(Params)), tl(tl(Params)))
					end;
				false -> notcommand
			end;
		true -> notcommand
	end.

desel(A, B) ->
	case lists:splitwith(fun(T) -> T /= $@ end, A) of
		{Cmd, [_|Sel]} -> {Cmd, B, Sel};
		{Cmd, []} -> {Cmd, B, []}
	end.

reply_channel(Nick, Channel) ->
	case config:require_value(config, [bot, nick]) of
		Channel -> Nick;
		_ -> Channel
	end.

reply_ping(#{id:=UserId}, _) ->
	case config:get_value(data, [call, UserId]) of
		'$none' -> [io_lib:format("<@!~p>: ", [UserId])];
		Nickname -> [Nickname, ": "]
	end.

describe_spec(Command, Spec) ->
	["Usage: ", Command, " ",
		string:join(lists:map(fun
				({_,ignore}) -> "(ignored)";
				(ignore) -> "(ignored)";
				({Name,_}) -> [$<,Name,$>];
				(Type) -> [$[,atom_to_list(Type),$]]
			end, Spec), " ")].

purespec(Spec) ->
	lists:map(fun
			({_Name,Type}) -> Type;
			(Type) -> Type
		end, Spec).

get_args(Spec, Args) ->
	case get_args(purespec(Spec), Args, []) of
		T when is_list(T) -> lists:reverse(T);
		T -> T
	end.


get_args([], [], X) -> X;
get_args([], _, _) -> {error, "Too many arguments provided."};
get_args(_, [], _) -> {error, "Not enough arguments provided."};

get_args([short|SRst],   [T|ARst], X) -> get_args(SRst,ARst,[T|X]);

get_args([ignore], _, X) -> X;
get_args([ignore|SRst], [_|ARst], X) -> get_args(SRst, ARst, X);

get_args([long], [], _) -> {error, "Not enough arguments provided."};
get_args([long], Rst, X) -> [string:join(Rst, " ") | X];
get_args([long|_], _, _) -> {error, "Invalid argument specification (bot bug)"};

get_args([list], [], _) -> {error, "Not enough arguments provided."};
get_args([list], Rst, X) -> [Rst | X];
get_args([list|_], _, _) -> {error, "Invalid argument specification (bot bug)"};

get_args([integer|SRst], [T|ARst], X) ->
	try
		Z = list_to_integer(T),
		get_args(SRst, ARst, [Z|X])
	catch
		error:badarg -> {error, io_lib:format("Invalid integer ~s", [T])}
	end;
get_args([Unk|_], _, _) -> {error, io_lib:format("Unknown or invalid argument type ~p (bot bug)", [Unk])}.

handle_command(Ranks, User, Channel = #{id:=ChannelID}, Guild, Command, Arguments, Selector) ->
	RP = reply_ping(User, Channel),
	Result = lists:foldl(fun
			(Rank, unhandled) ->
				case case config:get_value(temp, [bot, commands, Rank, string:to_lower(Command)]) of
					{_Mod, Func, ArgSpec} ->
						case get_args(ArgSpec, Arguments) of
							{error, T} ->
								core ! {respond, {message, ChannelID, [RP, "Error: ",T]}},
								error;
							X ->
								{Func, X}
						end;
					{_Mod, Func} -> {Func, Arguments};
					_ -> ok
				end of
					{Fn, Args} ->
						ParamMap = #{
								origin => User,
								channel => Channel,
								guild => Guild,
								ping => RP,
								params => Args,
								selector => Selector,
								ranks => Ranks
							},
						Fn(ParamMap);
					_ -> unhandled
				end;
			(_, Result) -> Result
		end, unhandled, Ranks),
	case Result of
		unhandled ->
			case alternate_commands([Command | Arguments]) of
				false -> ok;
				R ->
					RP = reply_ping(User, Channel),
					{respond, {message, ChannelID, [RP, R]}}
			end;
		_ -> Result
	end.

alternate_commands(Tokens) ->
	AltFunctions = lists:foldl(fun
			(Mod, Alt) ->
				Alt ++ util:call_or(Mod, alt_funcs, [], [])
		end, [], config:require_value(config, [bot, modules])),
	lists:foldl(fun
			(Func, false) ->
				case Func(Tokens) of
					false -> false;
					T ->
%						io:fwrite("~p(~p) returned ~p, HANDLED\n", [Func, Tokens, T]),
						T
				end;
			(_, Re) -> Re
		end, false, AltFunctions).

get_commands() ->
	[
		{"help", fun help/1, user},
		{"getids", fun ids/1, user}
	].

help(#{origin:=User = #{id:=UserID}, channel:=Channel, guild:=Guild, ping:=Ping, params:=Params}) ->
	case Params of
		[] ->
			Message = lists:foldl(fun(Rank, Rest) ->
					[Rest, help_list_commands(Rank, orddict:fetch_keys(config:get_value(temp, [bot, commands, Rank], [])))]
				end, [], permissions:rankof(User, Channel, Guild)),
			core ! {respond, {dm, UserID, Message}};
		_ ->
			{HelpCommand, _} = lists:foldl(fun
					(Module, {C, A}) -> util:call_or(Module, pre_command, [C,A], {C,A})
				end, {hd(Params), none}, config:require_value(config, [bot, modules])),
			HelpTopic = string:join([HelpCommand | tl(Params)], " "),
			case lists:foldl(fun
						(Rank, unhandled) ->
							case config:get_value(temp, [bot, commands, Rank, HelpCommand]) of
								{Mod, _, Spec} ->
									Strings = [describe_spec(HelpCommand, Spec) | util:call_or(Mod, get_help, [HelpTopic], [])],
									% core ! {respond, {dm, UserID, ["Help for '", HelpTopic, "':"]}},
									lists:foreach(fun(X) -> core ! {respond, {dm, UserID, X}} end, Strings);
								{Mod, _} ->
									case util:call_or(Mod, get_help, [HelpTopic], unhandled) of
										unhandled -> unhandled;
										Strings ->
											% core ! {respond, {dm, UserID, ["Help for '", HelpTopic, "':"]}},
											lists:foreach(fun(X) -> core ! {respond, {dm, UserID, X}} end, Strings),
											ok
									end;
								_ -> unhandled
							end;
						(_, Result) -> Result
					end, unhandled, permissions:rankof(User, Channel, Guild)) of
				unhandled -> ok;
				_ -> ok
			end
	end,
	ok.

ids(#{origin:=#{id:=UserID}, channel:=#{id:=ChannelID}, guild:=Guild, ping:=Ping}) ->
	case Guild of
		#{id:=GuildID} -> {respond, {message, ChannelID, [Ping, io_lib:format("user id: ~p; channel id: ~p; guild id: ~p", [UserID, ChannelID, GuildID])]}};
		_ -> {respond, {message, ChannelID, [Ping, io_lib:format("user id: ~p; channel id: ~p", [UserID, ChannelID])]}}
	end.

help_list_commands(_, []) -> ok;
help_list_commands(Rank, Commands) when length(Commands) < 35 ->
	[io_lib:format("\n~p commands: ",[Rank]), string:join(Commands, ", "), "."];
help_list_commands(Rank, Commands) ->
	{A, B} = lists:split(35, Commands),
	[io_lib:format("\n~p commands: ",[Rank]), string:join(A, ", "), ".", help_list_commands(Rank, B)].