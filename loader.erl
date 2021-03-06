-module(loader).

-export([load/0, compile_to/2]).

load() ->
	code:add_path("./_build/default/lib/gun/ebin"),
	code:add_path("./_build/default/lib/cowlib/ebin"),
	code:add_path("./_build/default/lib/ranch/ebin"),
	code:add_path("./bin"),
	compile_to("./bin", [
			bot,
			byond,
			common,
			complex,
			config,
			core,
			json,
			%lambda, % Missing core module?
			logging,
			mochijson,
			permissions,
			random_org,
			runtime_report,
			util
		]),
	code:add_path("./mod/bin"),
	compile_to("./mod/bin", [
			'mod/modules',
			'mod/command',
			'mod/eval',
			'mod/config_cmd',
			'mod/admin'
		]).

compile_to(Dir, Files) ->
	case lists:foldl(fun(File, {W,E}) ->
			case compile:file(File, [{outdir, Dir}, report_errors, report_warnings, return_errors, return_warnings]) of
				{ok, _, Warns} -> {Warns ++ W, E};
				{error, Errs, Warns} -> {Warns ++ W, Errs ++ E}
			end
		end, {[], []}, Files) of
		{_, []} -> ok;
		{_, _} -> error
	end.
