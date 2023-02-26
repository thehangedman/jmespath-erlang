%% @author: Andrey Tsirulev
%% @doc Erlang implementation of <a href="https://jmespath.org/">JMESPath</a> - a query language for JSON
-module(jmespath).

-export([
    parse/1,
    eval/2
]).

-export_type([jmespath/0, ast/0, json/0]).

-type ast() :: jmespath_parser:ast().
-type jmespath() :: {'jmespath', ast()}.
-type json() :: [json()] | #{binary() => json()} | binary() | number() | boolean() | 'null'.

-spec parse(binary()) -> jmespath().

%% @doc Parses JMESPath expression to AST tree.
parse(Binary) ->
    Tokens = jmespath_lexer:tokenize(Binary),
    Ast = jmespath_parser:parse(Tokens),
    {jmespath, Ast}.

-spec eval(binary() | jmespath(), json()) -> json().

%% @doc Evaluates JMESPath expression provided as string or a pre-parsed AST tree for a given JSON term.
eval(Binary, Json) when is_binary(Binary) ->
    eval(parse(Binary), Json);
eval({jmespath, Ast}, Json) ->
    jmespath_eval:eval(Ast, Json).
