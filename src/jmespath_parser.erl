%% @author: Andrey
%% @doc JMESPath parser
-module(jmespath_parser).

-export([
    parse/1
]).

-export_type([ast/0]).

-type ast() :: term().

-spec parse([jmespath_lexer:token()]) -> ast().

parse(Tokens) ->
    case expression(Tokens) of
        {Ast, []} -> Ast;
        {_, Tail} -> error({syntax, {unexpected_token, Tail}})
    end.

expression(Tokens) -> expression(Tokens, 0).

expression(Tokens, MinPriority) ->
    {Current, Tail} = chain(Tokens),
    expression1(Current, Tail, MinPriority, MinPriority).

-define(priority_cmp, 3).
-define(priority_and, 2).
-define(priority_or, 1).

expression1(Current, [C | Tokens], _, MinPriority) when C =:= '<'; C =:= '<='; C =:= '=='; C =:= '>='; C =:= '>'; C =:= '!=' ->
    {Expressions, Tail} = expression(Tokens, ?priority_cmp),
    expression1({C, Current, Expressions}, Tail, ?priority_cmp, MinPriority);
expression1(Current, ['&&' | Tokens], Priority, MinPriority) when Priority =< ?priority_and ->
    {Expr, Tail} = expression(Tokens, ?priority_and),
    expression1({'&&', Current, Expr}, Tail, ?priority_and, MinPriority);
expression1(Current, ['||' | Tokens], Priority, MinPriority) when Priority =< ?priority_or ->
    {Expr, Tail} = expression(Tokens, ?priority_or),
    expression1({'||', Current, Expr}, Tail, ?priority_or, MinPriority);
expression1(Current, ['|'|Tokens], 0, MinPriority) ->
    {Expressions, Tail} = expression(Tokens),
    expression1(as_list(Current,Expressions), Tail, 0, MinPriority);
expression1(Current, ['[]'|Tokens], 0, MinPriority) ->
    {Expressions, Tail} = next_in_chain(flatten, Tokens),
    expression1(as_list(as_list(Current),Expressions), Tail, 0, MinPriority);
expression1(Current, Tokens, Priority, MinPriority) when Priority > MinPriority ->
    expression1(Current, Tokens, Priority - 1, MinPriority);
expression1(Current, Tokens, MinPriority, MinPriority) ->
    {Current, Tokens}.

chain([FunctionName,'('|_] = Tokens) when is_binary(FunctionName) ->
    next_in_chain(function(Tokens));
chain([Identifier|Tokens]) when is_binary(Identifier) ->
    next_in_chain(Identifier, Tokens);
chain([{quoted, Identifier}|Tokens]) ->
    next_in_chain(Identifier, Tokens);
chain([{literal, Literal}|Tokens]) ->
    next_in_chain({literal, Literal}, Tokens);
chain([{raw_string, RawString}|Tokens]) ->
    next_in_chain({raw_string, RawString}, Tokens);
chain(['(' | Tokens]) ->
    {Expression, Tail} = expression(Tokens),
    next_in_chain(Expression, consume(')', Tail));
chain(['!' | Tokens]) ->
    {Expression, Tail} = chain(Tokens),
    next_in_chain({'!', Expression}, Tail);
chain(['[]' | Tokens]) ->
    next_in_chain(flatten, Tokens);
chain([B|_] = Tokens) when B =:= '['; B =:= '[?' ->
    next_in_chain(brackets(Tokens, true));
chain(['{'|_] = Tokens) ->
    next_in_chain(multi_select_hash(Tokens));
chain(['*' | Tokens]) ->
    next_in_chain(object_projection, Tokens);
chain(['@' | Tokens]) ->
    next_in_chain('@', Tokens);
chain([]) ->
    error({syntax, unexpected_eof});
chain([Token|_]) ->
    error({syntax, {unexpected_token, Token}}).

chain1(['.' | Tokens]) ->
    next_in_chain(sub_expression(Tokens));
chain1([B | _] = Tokens) when B =:= '['; B =:= '[?' ->
    next_in_chain(brackets(Tokens, false));
chain1(Tokens) ->
    {[], Tokens}.

next_in_chain({Expression, Tokens}) ->
    next_in_chain(Expression, Tokens).

next_in_chain(Expression, Tokens) ->
    {Expressions, Tail} = chain1(Tokens),
    {as_list(Expression,Expressions), Tail}.

sub_expression([FunctionName,'('|_] = Tokens) when is_binary(FunctionName) ->
    function(Tokens);
sub_expression([Identifier|Tokens]) when is_binary(Identifier) ->
    {Identifier, Tokens};
sub_expression([{quoted, Identifier}|Tokens]) ->
    {Identifier, Tokens};
sub_expression(['['|_] = Tokens) ->
    multi_select_list(Tokens);
sub_expression(['{'|_] = Tokens) ->
    multi_select_hash(Tokens);
sub_expression(['*'|Tokens]) ->
    {object_projection, Tokens};
sub_expression(Tokens) ->
    error({syntax, {invalid_sub_expression, Tokens}}).

brackets(['[',Index,']'|Tokens], _) when is_integer(Index) ->
    {Index, Tokens};
brackets(['[','*',']'|Tokens], _) ->
    {list_projection, Tokens};
brackets(['[',Start,':'|_] = Tokens, _) when is_integer(Start) ->
    slice_projection(Tokens);
brackets(['[',':'|_] = Tokens, _) ->
    slice_projection(Tokens);
brackets(['[?'|Tokens], _) ->
    {Expression, Tail} = expression(Tokens),
    {{filter, Expression}, consume(']', Tail)};
brackets(Tokens, true) ->
    multi_select_list(Tokens);
brackets(Tokens, false) ->
    error({syntax, {invalid_brackets, Tokens}}).

slice_projection(Tokens) ->
    {Slice, Tail} = slice(Tokens),
    {{slice, Slice}, Tail}.

slice(['[',Start,':',Stop,':',Step,']'|Tokens]) when is_integer(Start), is_integer(Stop), is_integer(Step) ->
    {{Start, Stop, Step}, Tokens};
slice(['[',Start,':',Stop,':',']'|Tokens]) when is_integer(Start), is_integer(Stop)->
    {{Start, Stop, 1}, Tokens};
slice(['[',Start,':',Stop,']'|Tokens]) when is_integer(Start), is_integer(Stop) ->
    {{Start, Stop, 1}, Tokens};
slice(['[',Start,':',':',Step,']'|Tokens]) when is_integer(Start), is_integer(Step) ->
    {{Start, default, Step}, Tokens};
slice(['[',Start,':',':',']'|Tokens]) when is_integer(Start) ->
    {{Start, default, 1}, Tokens};
slice(['[',Start,':',']'|Tokens]) when is_integer(Start) ->
    {{Start, default, 1}, Tokens};
slice(['[',':',Stop,':',Step,']'|Tokens]) when is_integer(Stop), is_integer(Step) ->
    {{default, Stop, Step}, Tokens};
slice(['[',':',Stop,':',']'|Tokens]) when is_integer(Stop) ->
    {{default, Stop, 1}, Tokens};
slice(['[',':',Stop,']'|Tokens]) when is_integer(Stop) ->
    {{default, Stop, 1}, Tokens};
slice(['[',':',':',Step,']'|Tokens]) when is_integer(Step) ->
    {{default, default, Step}, Tokens};
slice(['[',':',':',']'|Tokens]) ->
    {{default, default, 1}, Tokens};
slice(['[',':',']'|Tokens]) ->
    {{default, default, 1}, Tokens};
slice(Tokens) ->
    error({syntax, {invalid_slice, Tokens}}).

multi_select_list(['[',']'|_]) ->
    error({syntax, {unexpected_token, ']'}});
multi_select_list(['['|Tokens]) ->
    {List, Tail} = list(Tokens, ']', fun expression/1),
    {{multi_select_list, List}, Tail}.

keyval([Identifier, ':' | Tokens]) when is_binary(Identifier) ->
    {Expression, Tail} = expression(Tokens),
    {{Identifier, Expression}, Tail};
keyval([{quoted, Identifier}, ':' | Tokens]) ->
    {Expression, Tail} = expression(Tokens),
    {{Identifier, Expression}, Tail};
keyval(Tokens) ->
    error({syntax, {invalid_keyval, Tokens}}).

multi_select_hash(['{','}'|_]) ->
    error({syntax, {unexpected_token, '}'}});
multi_select_hash(['{'|Tokens]) ->
    {List, Tail} = list(Tokens, '}', fun keyval/1),
    {{multi_select_hash, maps:from_list(List)}, Tail}.

function([FunctionName, '(', ')' | Tokens]) ->
    {jmespath_functions:resolve_function(FunctionName, []), Tokens};
function([FunctionName, '(' | Tokens]) ->
    {List, Tail} = list(Tokens, ')', fun function_arg/1),
    {jmespath_functions:resolve_function(FunctionName, List), Tail}.

function_arg(['&' | Tokens]) ->
    {Expression, Tail} = expression(Tokens),
    {{'&', Expression}, Tail};
function_arg(Tokens) ->
    {Expression, Tail} = expression(Tokens),
    {Expression, Tail}.

list(Tokens, Terminator, Fun) ->
    case Fun(Tokens) of
        {Item, [Terminator|Tail]} ->
            {[Item], Tail};
        {Item, Tail} ->
            {Items, Tail1} = list(consume(',', Tail), Terminator, Fun),
            {[Item|Items], Tail1}
    end.            

consume(Token, [Token|Tokens]) -> Tokens;
consume(_Token, [ActualToken|_]) -> error({syntax, {unexpected_token, ActualToken}});
consume(_Token, []) -> error({syntax, unexpected_eof}).

as_list(H) when is_list(H) -> H;
as_list(H) -> [H].

as_list(H,[]) -> H;
as_list(H,T) when is_list(T) -> [H|T];
as_list(H,T) -> [H,T].

