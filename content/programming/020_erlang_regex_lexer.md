Title: erllex - simple erlang sample lexer
Date: 2015-04-08 17:06
Tags: erlang, lexer, regex
Summary: Showing off Regular Expressions in Erlang and how to build a simple lexer

A [lexer] analyses a character sequence and turns it into meaningful tokens.
As an example: if we want to build a simple calculator we would have to lex `<<"22 - 2011">>` into something like `[{number, 22}, {minus, -}, {number, 2011}]`.
There are different ways to build lexers.
Some just split on whitespace while others are more refined.
The easiest way to build something a bit more advanced is leveraging the power of regular expressions.

But enough with the theory.
Let's build a simple regex lexer in Erlang (while also tackling the issue of grepping subgroups in regex expressions with re)!

First, we don't want to type our rules all the time so let's quickly write up a function returning some for a simple calculator.
Remember to use two `\` since it is a escape character in Erlang as mentioned in [re-docs]:

    :::Erlang
    -module(erllex).
    -export([get_rules/0, tokenize/1, tokenize/2]).

    get_rules() -> 
        [
            "(?P<NUMBER>\\d+)",
            "(?P<IDENTIFIER>[a-zA-Z_]\\w+)",
            "(?P<PLUS>\\+)",
            "(?P<MINUS>\\-)",
            "(?P<WHITESPACE>\\s)"
        ].

Next is our main entry point.
`tokenize` takes the rules from earlier and turns them into a long, concatenated list and compiles it.
Notice the `anchored` option to only get the first match to the ruleset.

Additionally we are going to prepare a `namelist` via `re:inspect` so we can later determine which of our subgroup actually got matched.
For our rules it would be something like `{namelist,[<<"IDENTIFIER">>,<<"MINUS">>,<<"NUMBER">>,<<"PLUS">>,<<"WHITESPACE">>]}`, which is every name of every subgroup defined in `get_rules`.

Then we pass over to our main recursive loop `get_tokens` to actually do the lexing.

    :::Erlang
    tokenize(BinaryString) ->
        tokenize(BinaryString, erllex:get_rules()).

    tokenize(BinaryString, Rules) ->
        ConcatRules = concat_rules(Rules),
        {ok, CompiledRules} = re:compile(ConcatRules, [anchored]),
        {namelist, Namelist} = re:inspect(CompiledRules, namelist),
        get_tokens(BinaryString, CompiledRules, Namelist, []).

    concat_rules(NaiveRuleset) ->
        lists:foldl(
            fun(Rule, Acc) ->
                Rule ++ "|" ++ Acc
            end,
            [],
            NaiveRuleset
        ).

`get_tokens` is a standard recursive function and should look familiar to you if you did [functional programming] before.

The first clause covers our stop criteria.
When our `BinaryString` is empty we simply return everything in our accumulator after reversing it.
This is because we add new elements in the front of the list.
Have a look at [erlang-listhandling] for more information on why we do that.
Also have a look at `concat_rules` where we did the same but don't need to reverse since we don't care about the order.

If our input is not empty we `re:run` our compiled rules on it.
We need the `capture` option so we get a match for each subgroup defined in our rules.
Note that the result will not include the names of the subgroups, that is why we prepared `Namelist` which has the subgroup names in the same order as they are going to be returned by `re:run`.

As a next step we need to extract the matched subgroup via `extract_token`.
You can think of it as a special version of `lists:zip`.
As mentioned earlier the subgroup names in `Namelist` and the ones in `Matchlist` are in the same order.
The rule that actually matched will be nonempty while the others are going to be empty (`<<>>`).
Since we are only interested in the group that actually matched we drop empty `Matches` and stop as soon as we find one that is not.
This results in a returned tuple in the form of `{ok,{Identifier,Match}}` or, if you prefer, `{ok,{<<"NUMBER">>,<<"22">>}}`.

Before we recursively call `get_tokens` again we need to drop the `Match` from our `BinaryString` via a [Bit String Comprehension].
With the next string prepared and our token put into the accumulator we are set for the next recursion.

    :::Erlang
    get_tokens(<<>>, _Rules, _Namelist, Acc) ->
         lists:reverse(Acc);
    get_tokens(BinaryString, Rules, Namelist, Acc) ->
        {match, Matchlist} = re:run(BinaryString,Rules,[{capture,all_names,binary}]),
        {ok, Token} = extract_token(Namelist, Matchlist),
        {_, Matched} = Token,
        MatchedLength = byte_size(Matched),
        <<_Matched:MatchedLength/binary, NewBinaryString/binary>> = BinaryString,
        NewAcc = [Token | Acc],
        get_tokens(NewBinaryString, Rules, Namelist, NewAcc).

    extract_token([_Name | Namelist], [<<>> | Matchlist]) ->
        extract_token(Namelist, Matchlist);
    extract_token([Name | _Namelist], [Match | _Matchlist]) ->
        {ok, {Name, Match}};
    extract_token(_, _) ->
        {error, nomatch}.

After compilation (`erlc erllex.erl`) we can hop into `erl` and test it.

    :::erlang
    1> erllex:tokenize(<<"22-11">>).
    [{<<"NUMBER">>,<<"22">>},
     {<<"MINUS">>,<<"-">>},
     {<<"NUMBER">>,<<"11">>}]
    2> erllex:tokenize(<<"22 - Pi + 11">>).
    [{<<"NUMBER">>,<<"22">>},
     {<<"WHITESPACE">>,<<" ">>},
     {<<"MINUS">>,<<"-">>},
     {<<"WHITESPACE">>,<<" ">>},
     {<<"IDENTIFIER">>,<<"Pi">>},
     {<<"WHITESPACE">>,<<" ">>},
     {<<"PLUS">>,<<"+">>},
     {<<"WHITESPACE">>,<<" ">>},
     {<<"NUMBER">>,<<"11">>}]


The full source code can be found at [erllex].
Parser and formatter are next after extending the functionality of the lexer some more (different languages, ...).

As an additional Note: Erlang ships with a fully featured lexical analyzer generator called [leex].

[functional programming]: http://en.wikipedia.org/wiki/Functional_programming "Functional Programming on wikipedia"
[lexer]: http://en.wikipedia.org/wiki/Lexical_analysis "Lexical analysis on wikipedia"
[erllex]: https://github.com/ingwinlu/erllex "erllex on github"
[re-docs]: http://erlang.org/doc/man/re.html "re module docs"
[leex]: http://erlang.org/doc/man/leex.html "erlang leex"
[erlang-listhandling]: http://www.erlang.org/doc/efficiency_guide/listHandling.html "erlang listhandling"
[Bit String Comprehension]: http://erlang.org/doc/reference_manual/expressions.html#id81780

