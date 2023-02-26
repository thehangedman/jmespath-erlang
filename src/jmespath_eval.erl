%% @author: Andrey Tsirulev
%% @doc JMESPath evaluator
-module(jmespath_eval).

-export([
    eval/2
]).

-spec eval(jmespath:ast(), jmespath:json()) -> jmespath:json().

eval(_, null) -> null;
eval([], Json) -> Json;
eval([flatten|T], Json) -> list_projection(T, flatten(Json)); 
eval([{slice, Slice}|T], Json) -> list_projection(T, slice(Slice, Json));
eval([list_projection|T], Json) -> list_projection(T, Json);
eval([object_projection|T], Json) -> object_projection(T, Json);
eval([{filter, Filter}|T], Json) -> list_projection(T, filter(Filter, Json));
eval([H|T], Json) -> eval(T, eval(H,Json));
eval(flatten, Json) -> flatten(Json); 
eval({slice, Slice}, Json) -> slice(Slice, Json);
eval(list_projection, Json) -> list_projection([], Json);
eval(object_projection, Json) -> object_projection([], Json);
eval({filter, Filter}, Json) -> filter(Filter, Json);
eval('@', Json) -> Json;
eval({literal, Literal}, _) -> Literal;
eval(Identifier, Json) when is_binary(Identifier), is_map(Json) -> maps:get(Identifier, Json, null);
eval(Identifier, _Json) when is_binary(Identifier) -> null;
eval(Index, Json) when is_integer(Index) -> index(Index, Json);
eval({multi_select_list, Expressions}, Json) -> [ eval(Expression, Json) || Expression <- Expressions ];
eval({multi_select_hash, Hash}, Json) -> maps:map(fun(_,Expression) -> eval(Expression, Json) end, Hash);
eval({'||', Left, Right}, Json) -> '||'(Left, Right, Json);
eval({'&&', Left, Right}, Json) -> '&&'(Left, Right, Json);
eval({'!', Expr}, Json) -> '!'(Expr, Json);
eval({'<', Left, Right}, Json) -> compare(fun erlang:'<'/2, Left, Right, Json);
eval({'<=', Left, Right}, Json) -> compare(fun erlang:'=<'/2, Left, Right, Json);
eval({'>', Left, Right}, Json) -> compare(fun erlang:'>'/2, Left, Right, Json);
eval({'>=', Left, Right}, Json) -> compare(fun erlang:'>='/2, Left, Right, Json);
eval({'==', Left, Right}, Json) -> '=='(Left, Right, Json);
eval({'!=', Left, Right}, Json) -> '!='(Left, Right, Json);
eval({function, Name, Args}, Json) -> function(Name, Args, Json);
eval({variadic, Name, Args}, Json) -> variadic(Name, Args, Json).

list_projection(Expressions, Json) when is_list(Json) -> lists:filtermap(fun(Item) -> filtermap_null(eval(Expressions, Item)) end, Json);
list_projection(_, _) -> null.

object_projection(Expressions, Json) when is_map(Json) -> list_projection(Expressions, maps:values(Json));
object_projection(_, _) -> null.

filtermap_null(null) -> false;
filtermap_null(Value) -> {true, Value}.

filter(Filter, Json) when is_list(Json) -> [ Item || Item <- Json, to_bool(eval(Filter, Item)) ];
filter(_, _) -> null.

flatten([H|T]) when is_list(H) -> H ++ flatten(T);
flatten([H|T]) -> [H|flatten(T)];
flatten([]) -> [];
flatten(_) -> null.

function_arg({'&', Expression}, _) -> {jmespath, Expression};
function_arg(Expression, Json) -> eval(Expression, Json).

function(Name, Args, Json) ->
    ArgValues = [function_arg(Arg, Json) || Arg <- Args],
    apply(jmespath_functions, Name, ArgValues).

variadic(Name, Args, Json) ->
    ArgValues = [eval(Arg, Json) || Arg <- Args],
    jmespath_functions:Name(ArgValues).

'||'(Left, Right, Json) ->
    LeftValue = eval(Left, Json),
    case to_bool(LeftValue) of
        true -> LeftValue;
        false -> eval(Right, Json)
    end.

'&&'(Left, Right, Json) ->
    LeftValue = eval(Left, Json),
    case to_bool(LeftValue) of
        true -> eval(Right, Json);
        false -> LeftValue
    end.

'!'(Expr, Json) ->
    not to_bool(eval(Expr, Json)).    

'=='(Left, Right, Json) ->
    eval(Left, Json) =:= eval(Right, Json).

'!='(Left, Right, Json) ->
    eval(Left, Json) =/= eval(Right, Json).

compare(Op, Left, Right, Json) ->
    LeftValue = eval(Left, Json),
    RightValue = eval(Right, Json),
    compare(Op, LeftValue, RightValue).

compare(_, Left, Right) when not is_number(Left); not is_number(Right) -> null;
compare(Op, Left, Right) -> Op(Left, Right).

to_bool([]) -> false;
to_bool(Map) when Map =:= #{} -> false;
to_bool(<<"">>) -> false;
to_bool(false) -> false;
to_bool(null) -> false;
to_bool(_) -> true.

index(Index, Json) when is_list(Json), is_integer(Index) ->
    Len = length(Json),
    if
        Index >= 0, Index < Len -> lists:nth(Index + 1, Json);
        Index < 0, Len + Index >= 0 -> lists:nth(Len + Index + 1, Json);
        true -> null
    end;
index(_Index, _Json) ->
    null.
    
slice({_Start,_Stop,0}, _Json) ->
    error({'invalid-value', zero_slice_step});
slice(_Slice, Json) when not is_list(Json) ->
    null;
slice(_Slice, []) ->
    [];
slice({Start,default,1}, Json) ->
    slice_tail(Start, Json);
slice({default,default,-1}, Json) ->
    lists:reverse(Json);
slice({Start,Stop,Step}, Json) when Step > 0 ->
    slice_forward(Start, Stop, Step, Json);
slice({Start,Stop,Step}, Json) when Step < 0 ->
    slice_backward(Start, Stop, Step, Json).

slice_tail(default, Json) -> Json;
slice_tail(Start, Json) when Start >= 0 -> slice_nthtail(Start, Json);
slice_tail(Start, Json) when Start < 0 -> slice_nthtail(Start + length(Json), Json).

slice_forward(Start, Stop, Step, Json) ->
    Len = length(Json),
    Start1 =
        if
            Start =:= default -> 0;
            Start >= 0 -> Start;
            Start < 0 -> Len + Start
        end,
    Stop1 =
        if
            Stop =:= default -> Len;
            Stop >= 0 -> Stop;
            Stop < 0 -> Len + Stop
        end,
    if
        Start1 < 0 -> [];
        Start1 >= Len -> [];
        Stop1 =< Start1 -> [];
        Stop1 >= Len, Step == 1 -> slice_nthtail(Start1, Json);
        true -> slice_sublist(Json, Stop1, Step, Start1)
    end.

slice_backward(Start, Stop, Step, Json) ->
    Len = length(Json),
    Start1 =
        if
            Start =:= default -> Len - 1;
            Start >= 0 -> min(Start, Len - 1);
            Start < 0 -> Len + Start
        end,
    Stop1 =
        if
            Stop =:= default -> 0;
            Stop >= 0 -> Stop;
            Stop < 0 -> Len + Stop
        end,
    if
        Start1 < 0 -> [];
        Stop1 >= Start1 -> [];
        true -> slice_sublist(lists:reverse(Json), Len - Stop1 - 1, -Step, Len - Start1 - 1)
    end.

slice_nthtail(N, [_|T]) when N > 0 -> slice_nthtail(N - 1, T);
slice_nthtail(0, L) when is_list(L) -> L;
slice_nthtail(_, []) -> [].
 
slice_sublist([H|T], Stop, Step, 0) when Stop > 0 -> [H|slice_sublist(T, Stop - 1, Step, Step - 1)];
slice_sublist([_|T], Stop, Step, Skip) when Stop > 0 -> slice_sublist(T, Stop - 1, Step, Skip - 1);
slice_sublist(_, _, _, _) -> [].