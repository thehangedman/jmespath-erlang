%% @author: Andrey
%% @date: 23.02.2023

-module(jmespath_SUITE).

 -include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    basic/1,
    benchmarks/1,
    boolean/1,
    current/1,
    escape/1,
    filters/1,
    functions/1,
    identifiers/1,
    indices/1,
    literal/1,
    multiselect/1,
    pipe/1,
    slice/1,
    syntax/1,
    unicode/1,
    wildcard/1
]).

all() ->
    [basic, benchmarks, boolean, current, escape, filters, functions, identifiers, indices, literal,
        multiselect, pipe, slice, syntax, unicode, wildcard].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic(Config) ->
    run_file("basic.json", Config).

benchmarks(Config) ->
    run_file("benchmarks.json", Config).

boolean(Config) ->
    run_file("boolean.json", Config).

current(Config) ->
    run_file("current.json", Config).

escape(Config) ->
    run_file("escape.json", Config).

filters(Config) ->
    run_file("filters.json", Config).

functions(Config) ->
    run_file("functions.json", Config).

identifiers(Config) ->
    run_file("identifiers.json", Config).

indices(Config) ->
    run_file("indices.json", Config).

literal(Config) ->
    run_file("literal.json", Config).

multiselect(Config) ->
    run_file("multiselect.json", Config).

pipe(Config) ->
    run_file("pipe.json", Config).

slice(Config) ->
    run_file("slice.json", Config).

syntax(Config) ->
    run_file("syntax.json", Config).

unicode(Config) ->
    run_file("unicode.json", Config).

wildcard(Config) ->
    run_file("wildcard.json", Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_file(FileName, Config) ->
    FilePath = filename:join(?config(data_dir, Config), FileName),
    ct:pal(info, "File: ~s~n", [FilePath]),
    {ok, Binary} = file:read_file(FilePath),
    Tests = jsx:decode(Binary),
    Results = [ run_case(Given, Case) || #{<<"given">> := Given, <<"cases">> := Cases} <- Tests, Case <- Cases ],
    Total = length(Results),
    case Total - lists:sum(Results) of
        0 -> ct:pal(info, "~p of ~p tests succeeded", [Total, Total]);
        Failed -> ct:fail("~p of ~p tests failed", [Failed, Total])
    end.

run_case(Given, #{<<"expression">> := Expression, <<"result">> := Result}) ->
    try jmespath:eval(Expression, Given) of
        Result ->
            1;
        ActualResult ->
            ct:pal(error, "Given: ~s~nExpression: ~s~nExpected result: ~s~nActual result: ~p~n", [jsx:encode(Given), Expression, jsx:encode(Result), ActualResult]),
            0
    catch
        _:Error:Stacktrace ->
            ct:pal(error, "Given: ~s~nExpression: ~s~nExpected result: ~s~nError: ~p~nStacktrace: ~p~n", [jsx:encode(Given), Expression, jsx:encode(Result), Error, Stacktrace]),
            0
    end;
run_case(Given, #{<<"expression">> := Expression, <<"error">> := Error}) ->
    AtomError = binary_to_atom(Error),
    try jmespath:eval(Expression, Given) of
        Result -> 
            ct:pal(error, "Given: ~s~nExpression: ~s~nExpected error: ~s~nActual result: ~p~n", [jsx:encode(Given), Expression, Error, Result]),
            0
    catch
        error:{AtomError, _} -> 1;
        _:ResultError:Stacktrace ->
            ct:pal(error, "Given: ~s~nExpression: ~s~nExpected error: ~s~nActual result: ~p~nStacktrace: ~p~n", [jsx:encode(Given), Expression, Error, ResultError, Stacktrace]),
            0
    end;
run_case(Given, #{<<"expression">> := Expression, <<"bench">> := <<"parse">>}) ->
    try jmespath:parse(Expression) of
        _ -> 1
    catch
        _:Error:Stacktrace ->
            ct:pal(error, "Given: ~s~nExpression: ~s~nError: ~p~nStacktrace: ~p~n", [jsx:encode(Given), Expression, Error, Stacktrace]),
            0
    end;
run_case(Given, #{<<"expression">> := Expression, <<"bench">> := _}) ->
    try jmespath:eval(Expression, Given) of
        _ -> 1
    catch
        _:Error:Stacktrace ->
            ct:pal(error, "Given: ~s~nExpression: ~s~nError: ~p~nStacktrace: ~p~n", [jsx:encode(Given), Expression, Error, Stacktrace]),
            0
    end.
