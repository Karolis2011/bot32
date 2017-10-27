-module(bot).
-compile(export_all).
-compile({no_auto_import,[load_module/2]}).

-include("definitions.hrl").

get_commands() ->
	[
		{"update", fun(#{reply:=Reply}) -> bot ! {update, Reply} end, host}
	].

init() ->
	code:add_path("./mod/bin"),
	register(bot, self()),
	{SeedA,SeedB,SeedC}=erlang:timestamp(),
	random:seed(SeedA,SeedB,SeedC),

	util:waitfor(core),
	config:offer_value(config, [permissions], []),
	config:offer_value(config, [bot, modules], []),
	config:offer_value(config, [bot, client_id], "client_id_here"),
	config:offer_value(config, [bot, client_secret], "client_secret_here"),
	config:offer_value(config, [bot, token], "bot_token_here"),

	config:set_value(temp, [bot, commands], []),
	config:set_value(temp, [bot, guilds], []),

	 % wait for core to startup
	% case config:require_value(config, [bot, pass]) of
	% 	none -> ok;
	% 	Pass -> core ! {irc, {pass, Pass}}
	% end,
	% core ! {irc, {user, {config:require_value(config, [bot, user]), config:require_value(config, [bot, mode]), config:require_value(config, [bot, real])}}},
	% core ! {irc, {nick, config:require_value(config, [bot, nick])}},
	receive
		{event, 'READY', Data} -> handle_gateway_event('READY', Data), ok
	after
		3000 -> throw(connection_failed)
	end,
	logging:log(info, ?MODULE, "starting"),

	config:set_value(temp, [bot, commands], []),

	Modules = config:get_value(config, [bot, modules]),
	config:set_value(config, [bot, modules], []),
	logging:log(info, ?MODULE, "module load stat: ~s", [modules:load_modules(Modules)]),
	loop(),
	logging:log(info, ?MODULE, "stopping").

reinit(_) -> reinit().
reinit() ->
	register(bot, self()),
	logging:log(info, ?MODULE, "starting"),
	loop(),
	logging:log(info, ?MODULE, "stopping").

loop() ->
	case receive
		{event, EventName, Data} -> handle_gateway_event(EventName, Data);
		T when is_atom(T) -> T;
		{T, K} when is_atom(T) -> {T, K}
%		T -> logging:log(error, ?MODULE, "unknown receive ~p, continuing", [T])
	end of
		quit -> logging:log(info, ?MODULE, "Got quit command, quiting"), ok;
		error -> logging:log(info, ?MODULE, "Got error command, quiting"), error;
		ok -> bot:loop();
		update ->
			spawn(common,purge_call,[bot,reinit,[]]),
			ok;
		{update,Chan} ->
			spawn(common,purge_call_report,[bot,reinit,[],Chan]),
			ok;
		S -> logging:log(error, ?MODULE, "unknown code ~p, continuing", [S]), bot:loop()
	end.

handle_gateway_event('READY', #{session_id:=Session,user:=#{bot:=IsBot, username:=BotUsername, id:=BotId}}) ->
	config:set_value(temp, [bot, username], BotUsername),
	config:set_value(temp, [bot, id], BotId),
	config:set_value(temp, [bot, names], [binary_to_list(BotUsername), io_lib:format("<@~p>", [BotId])]), 
	ok;
handle_gateway_event('GUILD_CREATE', GuildObject = #{id:=GuildID, channels:=ChannelList}) ->
	config:set_value(temp, [bot, guilds, GuildID], GuildObject),
	lists:foreach(fun (Channel = #{id:=ChannelID}) ->
		config:set_value(temp, [bot, chanmap, ChannelID], GuildID),
		config:set_value(temp, [bot, channels, ChannelID], Channel)
	end, ChannelList),
	ok;
handle_gateway_event('CHANNEL_CREATE', Channel = #{id:=ChannelID}) ->
	config:set_value(temp, [bot, channels, ChannelID], Channel), ok;
handle_gateway_event('MESSAGE_CREATE', Data = #{channel_id:=ChannelID, author:=User = #{id:=UserID}, type:=Type, content:=Message}) ->
	MessageTokens = string:tokens(binary_to_list(Message), " "),
	BotId = config:get_value(temp, [bot, id]),
	case UserID of
		BotId -> ok;
		_ -> 
		case Type of
			0 -> 
			case config:get_value(temp, [bot, channels, ChannelID]) of
				'$none' -> error;
				Channel ->
					case config:get_value(temp, [bot, chanmap, ChannelID]) of
						'$none' -> 
							distribute_event(newMessage, {Data, MessageTokens, User, Channel, none});
						GuildID ->
							case config:get_value(temp, [bot, guilds, GuildID]) of
								'$none' -> error;
								Guild ->
									distribute_event(newMessage, {Data, MessageTokens, User, Channel, Guild})
							end
					end
			end;
			OT -> logging:log(info, ?MODULE, "Got unknown type (~p) message, ignoring", [OT]), ok
		end
	end;
handle_gateway_event('TYPING_START', _) -> ok;
handle_gateway_event('VOICE_STATE_UPDATE', _) -> ok;
handle_gateway_event('PRESENCE_UPDATE', _) -> ok;
handle_gateway_event('WEBHOOKS_UPDATE', _) -> ok;
handle_gateway_event(EventName, Data) ->
	logging:log(error, ?MODULE, "Unhandled event ~p: ~p", [EventName, Data]), ok.

% check_utf8(<<>>) -> true;
% check_utf8(<<_/utf8, B/binary>>) -> check_utf8(B);
% check_utf8(_) -> false. c
%
% handle_irc(ctcp, {action, Chan, User=#user{nick=Nick}, Tokens}) ->
% 	case permissions:hasperm(User, Chan, ignore) of
% 		true ->
% 			logging:log(ignore, ?MODULE, "Ignoring ~s!~s@~s ACTION: ~s.", [Nick, User#user.username, User#user.host, string:join(Tokens, " ")]),
% 			ok;
% 		false ->
% 			distribute_event(ctcp, {action, Chan, User, Tokens})
% 	end;
%
% handle_irc(msg, Params={User=#user{nick=Nick}, Channel, Tokens}) ->
% 	case lists:all(fun(T) -> check_utf8(list_to_binary(T)) end, Tokens) of
% 		false -> logging:log(utf8, ?MODULE, "Ignoring '~s' due to invalid UTF-8", [string:join(Tokens, " ")]);
% 		true ->
% 	case permissions:hasperm(User, ignore) of
% 		true ->
% 			logging:log(ignore, ?MODULE, "Ignoring ~s!~s@~s: ~s.", [Nick, User#user.username, User#user.host, string:join(Tokens, " ")]),
% 			distribute_event(msg_ignored, {User, Channel, Tokens}),
% 			ok;
% 		false ->
% 			distribute_event(msg, Params),
% 			case config:require_value(config, [bot, nick]) of
% 				Channel ->
% 					case permissions:hasperm(User, admin) of
% 						true -> ok;
% 						_ -> permissions:message_all_rank(["Query from ",Nick], string:join(Tokens, " "), pmlog)
% 					end;
% 				_ -> ok
% 			end
% 	end
% 	end;
%
% handle_irc(nick, {U=#user{nick=OldNick}, NewNick}) ->
% 	case config:get_value(config, [bot, nick]) of
% 		OldNick -> config:set_value(config, [bot, nick], NewNick);
% 		_ ->
% 			distribute_event(nick, {U, NewNick})
% 	end;
%
% handle_irc(notice, {Src, Trg, Msg}) ->
% 	logging:log(info, ?MODULE, "NOTICE received from ~p to ~s: ~s", [Src, Trg, string:join(Msg, " ")]),
% 	ok;
%
% handle_irc(numeric, {{rpl,away},_}) -> ok;
% handle_irc(numeric, {{A,B},Params}) ->
% 	logging:log(info, ?MODULE, "Numeric received: ~p_~p ~s", [A,B,string:join(Params," ")]),
% 	distribute_event(numeric, {{A,B}, Params});
%
% handle_irc(Type, Params) ->
% 	distribute_event(Type, Params).
%
distribute_event(Type, Params) ->
	lists:foreach(fun(Module) ->
			case catch util:call_or(Module, handle_event, [Type, Params], null) of
				{'EXIT', X} -> logging:log(error, ?MODULE, "~p:handle_event(~p, ~p) exited with ~p", [Module, Type, "<Prameters hidden>", X]);
				{'EXIT', RS, X} -> logging:log(error, ?MODULE, "~p:handle_event(~p, ~p) errored with ~p (~p)", [Module, Type, "<Prameters hidden>", X, RS]);
				_ -> ok
			end
		end, config:require_value(config, [bot, modules])).
