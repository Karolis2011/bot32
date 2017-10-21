-module(eval).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"eecho", fun eecho/1, eval},
		{"eval", gen_eval(fun eval/1), eval},
		{"evalstr", gen_eval_str(fun eval/1), eval},
		{"bash", fun bash/1, [long], eval},
		{"bashl", fun bashl/1, [long], eval},
		{"py", fun py/1, [long], eval},
		{"pyl", fun pyl/1, [long], eval},
		{"s", fun shl/1, eval},
		{"sdrop", fun sdrop/1, eval},
		{"serase", fun serase/1, eval},
		{"sshow", fun sshow/1, eval},
		{"maths", gen_eval(fun math/1), user},
		{"math", gen_eval(fun math/1), user},
		{"sym", fun sym/1, user},
		{"lsym", fun lsym/1, host}
	].

bash(#{channel:=#{id:=ChannelID}, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("args", Param),
	{respond, {message, ChannelID, io_lib:format("~s~1500s", [Ping, util:safe_os_cmd("bash -c \"$args\"")])}}.

py(#{channel:=#{id:=ChannelID}, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("script", Param),
	{respond, {message, ChannelID, io_lib:format("~s~1500s", [Ping, util:safe_os_cmd("python -c \"$script\"")])}}.

bashl(#{channel:=#{id:=ChannelID}, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("args", Param),
	RRaw = util:safe_os_cmd("bash -c \"$args\""),
	lines2channel(ChannelID, Ping, RRaw).

pyl(#{channel:=#{id:=ChannelID}, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("script", Param),
	RRaw = util:safe_os_cmd("python -c \"$script\""),
	lines2channel(ChannelID, Ping, RRaw).

lines2channel(ChannelID, Ping, RRaw) ->
	R = lists:flatmap(fun
			(10) -> [10];
			(9) -> "    ";
			(T) when T < 32 -> [];
			(T) -> [T]
		end, RRaw),
	{respond, {message, ChannelID, [Ping, R]}}.


eecho(Params=#{params:=Tokens}) ->
	ES = gen_eval_str(fun eval/1),
	NewParams = string:tokens(lists:flatten(["\"", string:join(Tokens, " "), "\""]), " "),
	ES(Params#{ping := [], params := NewParams}).

sym(#{channel:=#{id:=ChannelID}, ping:=P, params:=Par}) ->
	os:putenv("sym", string:join(Par, " ")),
	{respond, {message, ChannelID, [P, os:cmd("./sympy_eval.sh 5 \"$sym\"")]}}.
lsym(#{channel:=#{id:=ChannelID}, ping:=P, params:=Par}) ->
	os:putenv("sym", string:join(Par, " ")),
	{respond, {message, ChannelID, [P, os:cmd("./sympy_eval.sh 60 \"$sym\"")]}}.

shl(#{channel:=#{id:=ChannelID}, ping:=P, params:=Params}) ->
	PStr = lists:flatten(string:join(Params, " ")),
	String = case lists:last(PStr) of
		$. -> PStr;
		_ -> PStr ++ "."
	end,
	Bindings = config:get_value(data, [eval, shell], []),
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, Forms} ->
					case catch erl_eval:exprs(Forms, Bindings) of
						{value, Value, NewBinds} ->
							config:set_value(data, [eval, shell], NewBinds),
							{respond, {message, ChannelID, [P, io_lib:format("~500p", [Value])]}};
						{'EXIT', {Reason, Stack}} when is_list(Stack) -> {respond, {message, ChannelID, [P, format_reasonstack(Reason, Stack)]}};
						{'EXIT', Term} -> {respond, {message, ChannelID, [P, io_lib:format("Code exited with ~p", [Term])]}};
						Term -> {respond, {message, ChannelID, [P, io_lib:format("Code threw ~p", [Term])]}}
					end;
				T -> {respond, {message, ChannelID, [P, io_lib:format("~p", [T])]}}
			end;
		T -> {respond, {message, ChannelID, [P, io_lib:format("~p", [T])]}}
	end.

sshow(#{channel:=#{id:=ChannelID}, ping:=P}) ->
	case config:get_value(data, [eval, shell]) of
		'$none' -> {respond, {message, ChannelID, [P, "No state found."]}};
		V -> {respond, {message, ChannelID, [P, io_lib:format("~500p", [V])]}}
	end.

serase(#{channel:=#{id:=ChannelID}, ping:=P, params:=A}) when length(A) /= 1 -> {respond, {message, ChannelID, [P, "Provide a single var name."]}};
serase(#{channel:=#{id:=ChannelID}, ping:=P, params:=[Var]}) ->
	Msg = case config:get_value(data, [eval, shell]) of
		'$none' -> "No vars found.";
		Vars ->
			Atom = list_to_atom(Var),
			case orddict:find(Atom, Vars) of
				{ok, Val} ->
					config:set_value(data, [eval, shell], orddict:erase(Atom, Vars)),
					io_lib:format("Dropped value ~500p.", [Val]);
				error ->
					io_lib:format("No variable ~s found.", [Var])
			end
	end,
	{respond, {message, ChannelID, [P, Msg]}}.

sdrop(#{channel:=#{id:=ChannelID}, ping:=P}) ->
	config:set_value(data, [eval, shell], []),
	{respond, {message, ChannelID, [P, "State dropped."]}}.

gen_eval(Func) ->
	fun(#{channel:=#{id:=ChannelID},ping:=Ping,params:=[    ]}) -> {respond, {message, ChannelID, [Ping, "Provide a string to evaluate!"]}};
	   (#{channel:=#{id:=ChannelID},ping:=Ping,params:=Params}) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {respond, {message, ChannelID, [Ping, re:replace(io_lib:format("~w", [Value]), "[\r\n]", "")]}};
			{'EXIT', {Reason, Stack}} when is_list(Stack) -> {respond, {message, ChannelID, [Ping, format_reasonstack(Reason, Stack)]}};
			{'EXIT', Term} -> {respond, {message, ChannelID, [Ping, io_lib:format("Code exited with ~p", [Term])]}};
			{cerr, Term} -> {respond, {message, ChannelID, [Ping, Term]}};
			Term -> {respond, {message, ChannelID, [Ping, io_lib:format("Code threw ~p", [Term])]}}
		end
	end.

gen_eval_str(Func) ->
	fun(#{channel:=#{id:=ChannelID},ping:=Ping,params:=[    ]}) -> {respond, {message, ChannelID, [Ping, "Provide a string to evaluate!"]}};
	   (#{channel:=#{id:=ChannelID},ping:=Ping,params:=Params}) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {respond, {message, ChannelID, [Ping, re:replace(io_lib:format("~s", [Value]), "[\r\n]", "")]}};
			{'EXIT', {Reason, Stack}} when is_list(Stack) -> {respond, {message, ChannelID, [Ping, format_reasonstack(Reason, Stack)]}};
			{'EXIT', Term} -> {respond, {message, ChannelID, [Ping, io_lib:format("Code exited with ~p", [Term])]}};
			{cerr, Term} -> {respond, {message, ChannelID, [Ping, Term]}};
			Term -> {respond, {message, ChannelID, [Ping, io_lib:format("Code threw ~p", [Term])]}}
		end
	end.

format_reasonstack(Reason, [TopFrame|_]) ->
	io_lib:format("Error: ~p at ~p", [Reason, TopFrame]).

eval(String) ->
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, Forms} ->
					case erl_eval:exprs(Forms, []) of
						{value, Value, _} -> {ok, Value};
						X -> throw(X)
					end;
				{error,{_,_,Reason}} -> {cerr, Reason};
				X -> throw(X)
			end;
		{error, {_,_,A}, B} -> {cerr, [A, " before '", B, "'"]};
		X -> throw(X)
	end.


math(String) ->
	case re:run(String, "(^|[^a-zA-Z])fun($|[^a-zA-Z])", [{capture, none}]) of
		match -> {cerr, "Characters 'fun' detected in your input; quit trying to mess with this."};
		nomatch ->
			case erl_scan:string(String) of
				{ok, Tokens, _} ->
					case erl_parse:parse_exprs(Tokens) of
						{ok, [Form]} ->
							case erl_eval:expr(Form, [{'C',299792458}, {'E',2.718281828459}, {'I', {0,1}}, {'Pi', math:pi()}], {eval, fun lmatheval/3}, {value, fun nlmath/2}) of
								{value, Value, _} -> {ok, Value};
								X -> throw(X)
							end;
						{ok, _} -> {cerr, "Too many statements!"};
						{error,{_,_,Reason}} -> {cerr, Reason};
						X -> throw(X)
					end;
				{error, {_,_,A}, B} -> {cerr, [A, " before '", B, "'"]};
				X -> throw(X)
			end
	end.

lmatheval(Func, Args, Bindings) ->
	RealArgs = lists:map(fun(T) ->
			case erl_eval:expr(T, Bindings, {eval, fun lmatheval/3}, {value, fun nlmath/2}) of
				{value, Value, _} -> Value;
				X -> throw(X)
			end
		end, Args),
	{value, lmath(Func, RealArgs), Bindings}.


lmath(sum, [A]) when is_list(A) -> lists:foldl(fun erlang:'+'/2, 0, A);
lmath(avg, [A]) when is_list(A) -> lists:foldl(fun erlang:'+'/2, 0, A) / length(A);
lmath(ipow, [B,P]) when is_integer(B) andalso is_integer(P) -> integer_pow(B,P,1);
%lmath(fact, [A]) when is_integer(A) -> factorial(A,1);

lmath(real, [Z]) -> complex:real(Z);
lmath(imag, [Z]) -> complex:imag(Z);
lmath(conj, [Z]) -> complex:conj(Z);
lmath(cabs, [Z]) -> complex:abs(Z);
lmath(csqrt, [Z]) -> complex:sqrt(Z);
lmath(cpow, [Z,N]) -> complex:pow(Z,N);
lmath(root, [Z,N]) -> complex:root(Z,N);

lmath(Name, []) -> math:Name();
lmath(Name, [X]) -> math:Name(X);
lmath(Name, [X,Y]) -> math:Name(X,Y);
lmath(Name, _) -> ufunc(Name).


nlmath({erlang,T},[A]) ->
%	common:debug("debug", "erlang:~p(~p, ~p)", [T,A]),
	case T of
		'not' -> not A;
		'bnot' -> bnot A;
		'-' when is_tuple(A) -> complex:sub(0,A);
		'-' -> - A;
		'length' -> length(A);
		_ -> ufunc(erlang,T)
	end;

nlmath({erlang,T},[A,B]) ->
%	common:debug("debug", "erlang:~p(~p, ~p)", [T,A,B]),
	case T of
		'+' -> complex:add(A,B);
		'*' -> complex:mul(A,B);
		'/' -> complex:divd(A,B);
		'-' -> complex:sub(A,B);
		'div' -> A div B;
		'rem' -> A rem B;
		'and' -> A and B;
		'or' -> A or B;
		'xor' -> A xor B;
		'band' -> A band B;
		'bor' -> A bor B;
		'bxor' -> A bxor B;
		'bsl' -> A bsl B;
		'bsr' -> A bsr B;
		'==' ->	A == B;
		'/=' -> A /= B;
		'=<' -> A =< B;
		'<' -> A < B;
		'>=' -> A >= B;
		'>' -> A > B;
		'=:=' -> A =:= B;
		'=/=' -> A =/= B;
		'++' -> A ++ B;
		'--' -> A -- B;
		_ -> ufunc(erlang,T)
	end;

nlmath({A,B}, _) -> ufunc(A,B).

ufunc(Mod,Func) -> ufunc(lists:flatten(io_lib:format("~s:~s", [Mod, Func]))).
ufunc(Func) -> throw(lists:flatten(io_lib:format("Unknown function ~s",[Func]))).

% Extra math functions for &math

integer_pow(_,N,_) when N > 1024 -> throw("Power too large.");
integer_pow(_,0,Y) -> Y;
integer_pow(X,N,Y) -> integer_pow(X*X, N bsr 1, if (N band 1) == 1 -> Y*X; true -> Y end).
