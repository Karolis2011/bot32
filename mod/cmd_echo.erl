-module(cmd_echo).
-compile(export_all).

get_commands() ->
	[
		{"echo", fun echo/1, [long], user},
		{"npecho", fun npecho/1, [long], user}
	].

echo(#{channel:=#{id:=ChannelID}, ping:=Ping, params:=[String]}) ->
	{respond, {message, ChannelID, [Ping, String]}}.

npecho(#{channel:=#{id:=ChannelID}, params:=[String]}) ->
	{respond, {message, ChannelID, String}}.