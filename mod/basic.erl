-module(basic).
-compile(export_all).

get_commands() ->
	[
		{"ping", fun ping/1, user},
		{"pong", fun pong/1, user},
		{"rand", fun rand/1, [integer], user},
		{"pick", fun pick/1, [list], user},
		{"rot13", fun rot_thirteen/1, [{"string", long}], user},
		{"rot", fun rot_n/1, [integer, {"string", long}], user},
		{"coin", fun coin/1, user}
	].

rainbow() ->
	R = fun(Str) ->
			lists:map(fun({Chr,Col})->
					[3,if Col<10->$0;true->[]end,integer_to_list(Col),Chr]
				end,
				lists:zip(Str,
					lists:flatten(
						[lists:duplicate(length(Str) div 13, lists:seq(2,14)),
						lists:seq(2, 1 + (length(Str) rem 13))
						]
					)
				)
			) end.

alt_funcs() -> [fun select_or_string/1].

select_or_string(Tokens) ->
	case collapse_or_string(Tokens, [], []) of
		false -> false;
		[] -> false;
		[_] -> false;
		Options -> util:pick_rand(Options)
	end.

collapse_or_string([], [], _) -> false;
collapse_or_string([], COpt, Options) -> [COpt | Options];
collapse_or_string(["or"|_], [], _) -> false;
collapse_or_string(["or"|L], COpt, Options) -> collapse_or_string(L, [], [COpt | Options]);
collapse_or_string([T|L], [], Options) -> collapse_or_string(L, [T], Options);
collapse_or_string([T|L], COpt, Options) -> collapse_or_string(L, [COpt,32|T], Options).


i2l(T, S) when T < 10 -> [S] ++ integer_to_list(T);
i2l(T, _) -> integer_to_list(T).

ping(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping}) -> {respond, {message, ChannelID, [Ping, "Pong!"]}}.
pong(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping}) -> {respond, {message, ChannelID, [Ping, "Ping!"]}}.

rand(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping, params:=[Num]}) ->
	case Num > 0 of
		true -> {respond, {message, ChannelID, [Ping, erlang:integer_to_list(random:uniform(Num))]}};
		false -> {respond, {message, ChannelID, [Ping, "Please pass a positive integer."]}}
	end.

pick(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping, params:=[List]}) ->
	{respond, {message, ChannelID, [Ping, util:pick_rand(List)]}}.

h(Nick) -> h(Nick, 0).
h([],T) -> T;
h([H|R],T) -> h(R,T*37+H).

rot_thirteen(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping, params:=[String]}) ->
	Rotated = lists:map(fun(T) ->
		if
			T >= $A andalso T =< $M -> T+13;
			T >= $N andalso T =< $Z -> T-13;
			T >= $a andalso T =< $m -> T+13;
			T >= $n andalso T =< $z -> T-13;
			true -> T
		end end, String),
	{respond, {message, ChannelID, [Ping, Rotated]}}.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

rot_n(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping, params:=[N, String]}) ->
	Rotated = lists:map(fun(T) ->
		if
			T >= $A andalso T =< $Z -> $A + mod(T - $A + N, 26);
			T >= $a andalso T =< $z -> $a + mod(T - $a + N, 26);
			true -> T
		end end, String),
	{respond, {message, ChannelID, [Ping, Rotated]}}.

coin(#{channel:=Channel = #{id:=ChannelID}, ping:=Ping}) ->
	{respond, {message, ChannelID, [Ping, lists:nth(random:uniform(2), ["Heads!", "Tails!"])]}}.
