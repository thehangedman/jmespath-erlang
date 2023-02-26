%% @author: Andrey
%% @doc JMESPath lexer
-module(jmespath_lexer).

-export([tokenize/1]).

-export_type([token/0]).

-type token() :: atom() | integer() | binary() | {'quoted', binary()} | {'literal', jmespath:json()}.

-define(is_ascii(C), ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z))).
-define(is_digit(C), (C >= $0 andalso C =< $9)).

-spec tokenize(binary()) -> [token()].

tokenize(<<$.,Tail/binary>>) -> ['.'|tokenize(Tail)];
tokenize(<<$*,Tail/binary>>) -> ['*'|tokenize(Tail)];
tokenize(<<$,,Tail/binary>>) -> [','|tokenize(Tail)];
tokenize(<<$:,Tail/binary>>) -> [':'|tokenize(Tail)];
tokenize(<<$@,Tail/binary>>) -> ['@'|tokenize(Tail)];
tokenize(<<$(,Tail/binary>>) -> ['('|tokenize(Tail)];
tokenize(<<$),Tail/binary>>) -> [')'|tokenize(Tail)];
tokenize(<<${,Tail/binary>>) -> ['{'|tokenize(Tail)];
tokenize(<<$},Tail/binary>>) -> ['}'|tokenize(Tail)];
tokenize(<<$[,$],Tail/binary>>) -> ['[]'|tokenize(Tail)];
tokenize(<<$[,$?,Tail/binary>>) -> ['[?'|tokenize(Tail)];
tokenize(<<$[,Tail/binary>>) -> ['['|tokenize(Tail)];
tokenize(<<$],Tail/binary>>) -> [']'|tokenize(Tail)];
tokenize(<<$|,$|,Tail/binary>>) -> ['||'|tokenize(Tail)];
tokenize(<<$|,Tail/binary>>) -> ['|'|tokenize(Tail)];
tokenize(<<$&,$&,Tail/binary>>) -> ['&&'|tokenize(Tail)];
tokenize(<<$&,Tail/binary>>) -> ['&'|tokenize(Tail)];
tokenize(<<$<,$=,Tail/binary>>) -> ['<='|tokenize(Tail)];
tokenize(<<$<,Tail/binary>>) -> ['<'|tokenize(Tail)];
tokenize(<<$>,$=,Tail/binary>>) -> ['>='|tokenize(Tail)];
tokenize(<<$>,Tail/binary>>) -> ['>'|tokenize(Tail)];
tokenize(<<$=,$=,Tail/binary>>) -> ['=='|tokenize(Tail)];
tokenize(<<$!,$=,Tail/binary>>) -> ['!='|tokenize(Tail)];
tokenize(<<$!,Tail/binary>>) -> ['!'|tokenize(Tail)];
tokenize(<<C,_/binary>> = Binary) when ?is_ascii(C); C =:= $_ ->
    {Identifier, Tail} = identifier(Binary),
    [Identifier|tokenize(Tail)];
tokenize(<<$-,C,Tail/binary>>) when ?is_digit(C) ->
    {Number, Tail1} = number(Tail, C - $0),
    [-Number|tokenize(Tail1)];
tokenize(<<C,Tail/binary>>) when ?is_digit(C) ->
    {Number, Tail1} = number(Tail, C - $0),
    [Number|tokenize(Tail1)];
tokenize(<<$\",_/binary>> = Binary) ->
    {Identifier, Tail1} = quoted_identifier(Binary),
    [{quoted,Identifier}|tokenize(Tail1)];
tokenize(<<$`,_/binary>> = Binary) ->
    {Json, Tail} = json_literal(Binary),
    [{literal,Json}|tokenize(Tail)];
tokenize(<<$',_/binary>> = Binary) ->
    {RawString, Tail} = raw_string(Binary),
    [{literal,RawString}|tokenize(Tail)];
tokenize(<<C,Tail/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C=:= $\r ->
    tokenize(Tail);
tokenize(<<>>) ->
    [];
tokenize(Binary) ->
    error({syntax, {invalid_token, Binary}}).

identifier(<<_,Tail/binary>> = Binary) ->
    {Len, Tail1} = identifier(Tail, 1),
    {binary:part(Binary, 0, Len), Tail1}.

identifier(<<C,Tail/binary>>, Len) when ?is_ascii(C); C =:= $_; ?is_digit(C) ->
    identifier(Tail, Len + 1);
identifier(Binary, Len) ->
    {Len, Binary}.

number(<<C,Tail/binary>>, Result) when ?is_digit(C) ->
    number(Tail, Result * 10 + (C - $0));
number(Binary, Result) ->
    {Result, Binary}.

json_literal(<<$`,Binary/binary>>) ->
    {IoList, Tail} = json_literal_iolist(Binary),
    JsonString = list_to_binary(IoList),
    try
        Json = jsx:decode(JsonString),
        {Json, Tail}
    catch
        _:_ -> error({syntax, {invalid_json_literal, JsonString}})
    end.        

json_literal_iolist(Binary) ->
    case binary:match(Binary, [<<$\\,$`>>, <<$`>>]) of
        {Pos, 1} ->
            Part1 = binary:part(Binary, 0, Pos),
            Tail = binary:part(Binary, Pos + 1, byte_size(Binary) - Pos - 1),
            {[Part1], Tail};
        {Pos, 2} -> 
            Part1 = binary:part(Binary, 0, Pos),
            Tail = binary:part(Binary, Pos + 2, byte_size(Binary) - Pos - 2),
            {Part2, Tail1} = json_literal_iolist(Tail),
            {[Part1,$`|Part2], Tail1};
        nomatch ->
            error({syntax, unterminated_json_literal})
    end.

raw_string(<<$',Binary/binary>>) ->
    {IoList, Tail} = raw_string_iolist(Binary),
    RawString = list_to_binary(IoList),
    {RawString, Tail}.

raw_string_iolist(Binary) ->
    case binary:match(Binary, [<<$\\,$'>>, <<$\\,$\\>>, <<$'>>]) of
        {Pos, 1} ->
            Part1 = binary:part(Binary, 0, Pos),
            Tail = binary:part(Binary, Pos + 1, byte_size(Binary) - Pos - 1),
            {[Part1], Tail};
        {Pos, 2} -> 
            Part1 = binary:part(Binary, 0, Pos),
            Tail = binary:part(Binary, Pos + 2, byte_size(Binary) - Pos - 2),
            {Part2, Tail1} = raw_string_iolist(Tail),
            case binary:at(Binary, Pos + 1) of
                $\\ -> {[Part1,$\\,$\\|Part2], Tail1};
                $' -> {[Part1,$'|Part2], Tail1}
            end;
        nomatch ->
            error({syntax, unterminated_raw_string_literal})
    end.

quoted_identifier(<<$\",Binary/binary>>) ->
    quoted_identifier(Binary, []).

quoted_identifier(<<$\", Tail/binary>>, Result) ->
    {unicode:characters_to_binary(lists:reverse(Result)), Tail};
quoted_identifier(<<$\\, $\", Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\" | Result]);
quoted_identifier(<<$\\, $\\, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\\ | Result]);
quoted_identifier(<<$\\, $/, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$/ | Result]);
quoted_identifier(<<$\\, $b, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\b | Result]);
quoted_identifier(<<$\\, $f, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\f | Result]);
quoted_identifier(<<$\\, $n, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\n | Result]);
quoted_identifier(<<$\\, $r, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\r | Result]);
quoted_identifier(<<$\\, $t, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [$\t | Result]);
quoted_identifier(<<$\\, $u, F, A, B, C, $\\, $u, G, X, Y, Z, Tail/binary>>, Result) when 
    (A == $8 orelse A == $9 orelse A == $a orelse A == $b orelse A == $A orelse A == $B),
    (X == $c orelse X == $d orelse X == $e orelse X == $f orelse X == $C orelse X == $D orelse X == $E orelse X == $F),
    (F == $d orelse F == $D),
    (G == $d orelse G == $D) ->
    High = erlang:list_to_integer([$d, A, B, C], 16),
    Low = erlang:list_to_integer([$d, X, Y, Z], 16),
    Codepoint = (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000,
    quoted_identifier(Tail, [Codepoint | Result]);
quoted_identifier(<<$\\, $u, A, B, C, D, Tail/binary>>, Result) ->
    Codepoint = erlang:list_to_integer([A, B, C, D], 16),
    quoted_identifier(Tail, [Codepoint | Result]);
quoted_identifier(<<$\\, _/binary>> = Binary, _Result) ->
    error({syntax, {invalid_escape_syntax, Binary}});
quoted_identifier(<<C/utf8, Tail/binary>>, Result) ->
    quoted_identifier(Tail, [C|Result]);
quoted_identifier(<<>>, _Result) ->
    error({syntax, unterminated_quoted_identifier}).