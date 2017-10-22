-module(units).
-compile(export_all).

alt_funcs() ->
	[
		fun units_alt/1
	].

get_commands() ->
	[
		{"units", fun units/1, user}
	].

units(#{channel:=#{id:=ChannelID}, ping:=P, params:=Params}) ->
	case case lists:splitwith(fun(T)->T /= "in" andalso T /= "to" andalso T /= "->" end, Params) of
		{Src, []} ->
			util:unicode_os_putenv("units_src", string:join(Src, " ")),
			one;
		{[], [_]} -> "Provide a unit to convert from and a unit to convert to!";
		{[], _} -> "Provide a unit to convert from!";
		{_, [_]} -> "Provide a unit to convert to!";
		{Src, [_|Dst]} ->
			util:unicode_os_putenv("units_src", string:join(Src, " ")),
			util:unicode_os_putenv("units_dst", string:join(Dst, " ")),
			two
	end of
		one -> {respond, {message, ChannelID, [P, get_units_reply("units -t -- \"$units_src\"")]}};
		two -> {respond, {message, ChannelID, [P, get_units_reply("units -t -- \"$units_src\" \"$units_dst\"")]}};
		T -> {respond, {message, ChannelID, [P, T]}}
	end.

units_alt(Params) ->
	case lists:splitwith(fun(T) -> T /= "in" andalso T /= "to" andalso T /= "->" end, Params) of
		{Src, [_|Dst]} ->
			util:unicode_os_putenv("units_src", string:join(Src, " ")),
			util:unicode_os_putenv("units_dst", string:join(Dst, " ")),
			get_units_reply("units -t \"$units_src\" \"$units_dst\"");
		_ -> false
	end.

get_units_reply(Cmd) ->
	Ret = util:safe_os_cmd(Cmd),
	Toks = string:tokens(Ret, "\n"),
	string:join(Toks, "; ").
