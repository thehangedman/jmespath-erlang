%% @author: Andrey Tsirulev
%% @doc JMESPath built-in functions
-module(jmespath_functions).

-export([
    resolve_function/2,
    abs/1,
    avg/1,
    contains/2,
    ceil/1,
    ends_with/2,
    floor/1,
    join/2,
    keys/1,
    length/1,
    map/2,
    max/1,
    max_by/2,
    merge/1,
    min/1,
    min_by/2,
    not_null/1,
    reverse/1,
    sort/1,
    sort_by/2,
    starts_with/2,
    sum/1,
    to_array/1,
    to_string/1,
    to_number/1,
    type/1,
    values/1
]).

resolve_function(Identifier, Args) ->
    case function_info(Identifier) of
        {Name, Arity} when Arity =:= erlang:length(Args) -> {function, Name, Args};
        {Name, _} -> error({'invalid-arity', Name});
        {Name, variadic, MinArity} when erlang:length(Args) >= MinArity -> {variadic, Name, Args};
        {Name, variadic, _} -> error({'invalid-arity', Name})
    end.

function_info(<<"abs">>) -> {abs, 1};
function_info(<<"avg">>) -> {avg, 1};
function_info(<<"contains">>) -> {contains, 2};
function_info(<<"ceil">>) -> {ceil, 1};
function_info(<<"ends_with">>) -> {ends_with, 2};
function_info(<<"floor">>) -> {floor, 1};
function_info(<<"join">>) -> {join, 2};
function_info(<<"keys">>) -> {keys, 1};
function_info(<<"length">>) -> {length, 1};
function_info(<<"map">>) -> {map, 2};
function_info(<<"max">>) -> {max, 1};
function_info(<<"max_by">>) -> {max_by, 2};
function_info(<<"merge">>) -> {merge, variadic, 0};
function_info(<<"min">>) -> {min, 1};
function_info(<<"min_by">>) -> {min_by, 2};
function_info(<<"not_null">>) -> {not_null, variadic, 1};
function_info(<<"reverse">>) -> {reverse, 1};
function_info(<<"sort">>) -> {sort, 1};
function_info(<<"sort_by">>) -> {sort_by, 2};
function_info(<<"starts_with">>) -> {starts_with, 2};
function_info(<<"sum">>) -> {sum, 1};
function_info(<<"to_array">>) -> {to_array, 1};
function_info(<<"to_string">>) -> {to_string, 1};
function_info(<<"to_number">>) -> {to_number, 1};
function_info(<<"type">>) -> {type, 1};
function_info(<<"values">>) -> {values, 1};
function_info(Name) -> error({'unknown-function', Name}).

abs(Value) when is_number(Value) -> erlang:abs(Value);
abs(_) -> invalid_type(?FUNCTION_NAME).

avg([]) -> null;
avg(Elements) when is_list(Elements) -> float(lists:sum(number_list(?FUNCTION_NAME, Elements))) / erlang:length(Elements);
avg(_) -> invalid_type(?FUNCTION_NAME).

contains(Subject, Search) when is_list(Subject) -> lists:member(Search, Subject);
contains(Subject, Search) when is_binary(Subject), is_binary(Search) -> binary:match(Subject, Search) =/= nomatch;
contains(Subject, _Search) when is_binary(Subject) -> false;
contains(_, _) -> invalid_type(?FUNCTION_NAME).

ceil(Value) when is_number(Value) -> erlang:ceil(Value);
ceil(_) -> invalid_type(?FUNCTION_NAME).

ends_with(Subject, Prefix) when is_binary(Subject), is_binary(Prefix) ->
    byte_size(Prefix) =< byte_size(Subject) andalso binary:part(Subject, byte_size(Subject) - byte_size(Prefix), byte_size(Prefix)) =:= Prefix;
ends_with(_Subject, _Prefix) -> invalid_type(?FUNCTION_NAME).

floor(Value) when is_number(Value) -> erlang:floor(Value);
floor(_) -> invalid_type(?FUNCTION_NAME).

join(Glue, StringsArray) when is_binary(Glue), is_list(StringsArray) -> unicode:characters_to_binary(lists:join(Glue, string_list(?FUNCTION_NAME, StringsArray)));
join(_, _) -> invalid_type(?FUNCTION_NAME).

keys(Obj) when is_map(Obj) -> maps:keys(Obj);
keys(_) -> invalid_type(?FUNCTION_NAME).

length(Subject) when is_binary(Subject) -> string:length(Subject);
length(Subject) when is_list(Subject) -> erlang:length(Subject);
length(Subject) when is_map(Subject) -> maps:size(Subject);
length(_) -> invalid_type(?FUNCTION_NAME).

map({jmespath, Expr}, Elements) when is_list(Elements) -> lists:map(fun(E) -> jmespath_eval:eval(Expr, E) end, Elements);
map(_, _) -> invalid_type(?FUNCTION_NAME).

max([]) -> null;
max(List) when is_number(hd(List)) -> lists:max(number_list(?FUNCTION_NAME, List));
max(List) when is_binary(hd(List)) -> lists:max(string_list(?FUNCTION_NAME, List));
max(_) -> invalid_type(?FUNCTION_NAME).

max_by([], {jmespath, _}) -> null;
max_by(Elements, {jmespath, Expr}) when is_list(Elements) ->
    Tagged = [ {jmespath_eval:eval(Expr, E), E} || E <- Elements ],
    case hd(Tagged) of
        {Number, _} when is_number(Number) -> invalid_type_if_not_all(?FUNCTION_NAME, Tagged, fun({I,_}) -> is_number(I) end);
        {String, _} when is_binary(String) -> invalid_type_if_not_all(?FUNCTION_NAME, Tagged, fun({I,_}) -> is_binary(I) end);
        _ -> invalid_type(?FUNCTION_NAME)
    end,
    {_, Max} = lists:max(Tagged),
    Max;
max_by(_Elements, _Expr) -> invalid_type(?FUNCTION_NAME).

merge([H|T]) when is_map(H) -> maps:merge(H, merge(T));
merge([_|_]) -> invalid_type(?FUNCTION_NAME);
merge([]) -> #{}.

min([]) -> null;
min(List) when is_number(hd(List)) -> lists:min(number_list(?FUNCTION_NAME, List));
min(List) when is_binary(hd(List)) -> lists:min(string_list(?FUNCTION_NAME, List));
min(_) -> invalid_type(?FUNCTION_NAME).

min_by([], {jmespath, _}) -> null;
min_by(Elements, {jmespath, Expr}) when is_list(Elements) ->
    Tagged = [ {jmespath_eval:eval(Expr, E), E} || E <- Elements ],
    case hd(Tagged) of
        {Number, _} when is_number(Number) -> invalid_type_if_not_all(?FUNCTION_NAME, Tagged, fun({I,_}) -> is_number(I) end);
        {String, _} when is_binary(String) -> invalid_type_if_not_all(?FUNCTION_NAME, Tagged, fun({I,_}) -> is_binary(I) end);
        _ -> invalid_type(?FUNCTION_NAME)
    end,
    {_, Min} = lists:min(Tagged),
    Min;
min_by(_Elements, _Expr) -> invalid_type(?FUNCTION_NAME).

not_null([null|T]) -> not_null(T);
not_null([H|_]) -> H;
not_null([]) -> null.

reverse(Argument) when is_list(Argument) -> lists:reverse(Argument);
reverse(Argument) when is_binary(Argument) -> unicode:characters_to_binary(string:reverse(Argument));
reverse(_) -> invalid_type(?FUNCTION_NAME).

sort([]) -> [];
sort(List) when is_number(hd(List)) -> lists:sort(number_list(?FUNCTION_NAME, List));
sort(List) when is_binary(hd(List)) -> lists:sort(string_list(?FUNCTION_NAME, List));
sort(_) -> invalid_type(?FUNCTION_NAME).

sort_by([], {jmespath, _}) -> [];
sort_by(Elements, {jmespath, Expr}) when is_list(Elements) ->
    Tagged = [ {jmespath_eval:eval(Expr, E), E} || E <- Elements ],
    case hd(Tagged) of
        {Number, _} when is_number(Number) -> invalid_type_if_not_all(?FUNCTION_NAME, Tagged, fun({I,_}) -> is_number(I) end);
        {String, _} when is_binary(String) -> invalid_type_if_not_all(?FUNCTION_NAME, Tagged, fun({I,_}) -> is_binary(I) end);
        _ -> invalid_type(?FUNCTION_NAME)
    end,
    [ Value || {_,Value} <- lists:keysort(1, Tagged) ];
sort_by(_, _) -> invalid_type(?FUNCTION_NAME).

starts_with(Subject, Prefix) when is_binary(Subject), is_binary(Prefix) -> 
    byte_size(Prefix) =< byte_size(Subject) andalso binary:part(Subject, 0, byte_size(Prefix)) =:= Prefix;
starts_with(_Subject, _Prefix) -> invalid_type(?FUNCTION_NAME).

sum([]) -> 0;
sum(Elements) when is_list(Elements) -> lists:sum(number_list(?FUNCTION_NAME, Elements));
sum(_) -> invalid_type(?FUNCTION_NAME).

to_array(Arg) when is_list(Arg) -> Arg;
to_array(Arg) -> [Arg].

to_string(Arg) when is_binary(Arg) -> Arg;
to_string(Arg) -> jsx:encode(Arg).

to_number(Arg) when is_number(Arg) -> Arg;
to_number(Arg) when is_binary(Arg) ->
    try jsx:decode(Arg) of
        Number when is_number(Number) -> Number;
        _ -> null
    catch
        error:_ -> null
    end;
to_number(_) -> null.

type(Subject) when is_number(Subject) -> <<"number">>;
type(Subject) when is_binary(Subject) -> <<"string">>;
type(Subject) when is_boolean(Subject) -> <<"boolean">>;
type(Subject) when is_list(Subject) -> <<"array">>;
type(Subject) when is_map(Subject) -> <<"object">>;
type(null) -> <<"null">>.

values(Obj) when is_map(Obj) -> maps:values(Obj);
values(_) -> invalid_type(?FUNCTION_NAME).

invalid_type(FunctionName) -> error({'invalid-type', FunctionName}).

invalid_type_if_not_all(FunctionName, List, Pred) ->
    case lists:all(Pred, List) of
        true -> List;
        false -> invalid_type(FunctionName)
    end.

number_list(FunctionName, List) ->
    invalid_type_if_not_all(FunctionName, List, fun is_number/1).

string_list(FunctionName, List) ->
    invalid_type_if_not_all(FunctionName, List, fun is_binary/1).
