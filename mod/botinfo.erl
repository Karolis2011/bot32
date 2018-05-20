-module(botinfo).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"uptime", fun uptime/1, user},
		{"version", fun version/1, user},
		{"source", fun source/1, user},
		{"github", fun source/1, user}
	].

handle_event(_, _) -> ok.

sectimestamp() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())).

initialise() ->
	config:set_value(temp, [botinfo], sectimestamp()).
deinitialise() ->
	config:del_value(temp, [botinfo]).

uptime(#{channel:=_ = #{id:=ChannelID}, ping:=Ping}) ->
	config:offer_value(temp, [botinfo], sectimestamp()),
	StartTime = config:require_value(temp, [botinfo]),
	NowTime = sectimestamp(),
	{respond, {message, ChannelID, [Ping, "I have been running for ", common:format_time_difference(NowTime - StartTime)]}}.

version(#{channel:=_ = #{id:=ChannelID}, ping:=Ping}) ->
	{respond, {message, ChannelID, [Ping, version_string()]}}.

source(#{channel:=_ = #{id:=ChannelID}, ping:=Ping}) ->
	{respond, {message, ChannelID, [Ping, "http://github.com/Karolis2011/bot32"]}}}.

version_string() ->
	% Erlang info
	ErlVer = erlang:system_info(otp_release),

	% System info
	{_, OSname} = os:type(),
	OSver = case os:version() of
		{Maj,Min,Pat} -> [integer_to_list(Maj), $., integer_to_list(Min), $., integer_to_list(Pat)];
		String -> String
	end,

	io_lib:format("~s running on Erlang ~s on ~p ~s.", [?VERSION, ErlVer, OSname, OSver]).
