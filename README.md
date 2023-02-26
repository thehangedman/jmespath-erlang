# jmespath-erlang

This is an Erlang implementation of [JMESPath](https://jmespath.org/) - a query language for JSON.
You can extract and transform elements from a JSON document.

`jmespath-erlang` library passes the JMESPath compliance test suite, but has not been battle-tested yet.

`jmespath-erlang` uses binaries for string representation.

JSON documents are represented as maps with binary keys. Most Erlang JSON libraries use the same representation by default.

## JMESPath example

JMESPath expression:
```
locations[?state == 'WA'].name | sort(@) | {WashingtonCities: join(', ', @)}
```

Input JSON:
```json
{
  "locations": [
    {"name": "Seattle", "state": "WA"},
    {"name": "New York", "state": "NY"},
    {"name": "Bellevue", "state": "WA"},
    {"name": "Olympia", "state": "WA"}
  ]
}
```

Result:
```json
{"WashingtonCities": "Bellevue, Olympia, Seattle"}
```

Learn more at [JMESPath Tutorial](https://jmespath.org/tutorial.html).

## Usage

```erlang
JMESPath = <<"locations[?state == 'WA'].name | sort(@) | {WashingtonCities: join(', ', @)}">>,
JSON = #{
  <<"locations">> => [
    #{<<"name">> => <<"Seattle">>, <<"state">> => <<"WA">>},
    #{<<"name">> => <<"New York">>, <<"state">> => <<"NY">>},
    #{<<"name">> => <<"Bellevue">>, <<"state">> => <<"WA">>},
    #{<<"name">> => <<"Olympia">>, <<"state">> => <<"WA">>}
  ]
},
Result = jmespath:eval(JMESPath, JSON),
#{<<"WashingtonCities">> := <<"Bellevue, Olympia, Seattle">>} = Result.
```

Using parsed expressions:

```erlang
ParsedJMESPath = jmespath:parse(JMESPath),
jmespath:eval(ParsedJMESPath, JSON).
```
