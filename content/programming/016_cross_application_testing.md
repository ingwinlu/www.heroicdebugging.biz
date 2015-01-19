Title: Testing Across Applications with EUnit
Date: 2014-12-26 18:42
Tags: erlang, OTP, EUnit

I am currently developing a distributed system for my Bachelor Thesis which utilizes a node controller application to distribute calculations from a client application towards node applications. For testing I wanted to use [EUnit], mostly because that's what I already knew and writing Unit Tests for my data module was easy and straightforward. 

When it came to testing how the applications played together however I ran into a wall. For a project of this size it did not really make sense to specify a new 'collecting' application, which would list the others as dependency and write my tests like that. Also i wanted to keep my simple project structure ('proj_name/node_controller', ...) which then containes the [Erlang OTP] applications.

The solution I came up with for now is an [escript] that forces a compile via `mode(compile).`. The other thing I added was `compile([nowarn_unused_function]).` because the compiler will complain about the unexported functions. The `main` function then simply calls `eunit:test(?MODULE).` to execute all specified tests. Combined with a Makefile that ensures the 'subprojects' are freshly compiled I can now easily write my test cases.

    :::erlang
    #!/usr/bin/env escript

    -module(dist_calc_tests).
    -compile([nowarn_unused_function]).
    -mode(compile).
    -include_lib("eunit/include/eunit.hrl").

    main(_Args) ->
        eunit:test(?MODULE).

    basic_node_controller_test_() ->
        {foreach, 
            fun startup_basic/0,
            fun shutdown_basic/1,
            [
                fun test_basic/1
            ]}.

    startup_someapps([]) ->
        self();
    startup_someapps([{PathToApp, AppName}|OtherApps]) ->
        {ok, CurrentCwd} = file:get_cwd(),
        ok = file:set_cwd(PathToApp ++ "/ebin"),
        ok = application:load(AppName),
        ok = application:start(AppName),
        ok = file:set_cwd(CurrentCwd),
        startup_someapps(OtherApps).

    shutdown_someapps([]) ->
        ok;
    shutdown_someapps([AppName|OtherApps]) ->
        ok = application:stop(AppName),
        ok = application:unload(AppName),
        shutdown_someapps(OtherApps).

    startup_basic() ->
        startup_someapps(
            [
                {"./node", node},
                {"./node_controller", node_controller},
                {"./client", client}
            ]).

    shutdown_basic(_Pid) ->
        shutdown_someapps([client, node, node_controller]).

    test_basic(_Pid) ->
        timer:sleep(1000),
        [
            ?_assertMatch(
                [
                    {"127.0.0.1:13832", {
                            node_controller_node,
                            "127.0.0.1",
                            13832,
                            _OpSet,
                            _Timer,
                            online,
                            0
                    }}
                ], node_controller_data_node:get_nodes()),
            ?_assertEqual(
                "login_success",
                client_app:login("alice","12345"))        
        ].

Please keep in mind this is only intended for very basic protocol testing, i.e. if i break something during refactoring or similar activities. In the future I will probably modify the startup functions so that I can also easily test out different env settings for more advanced scenarios.

[escript]: http://www.erlang.org/doc/man/escript.html "escript documentation"
[Erlang OTP]: http://www.erlang.org/doc/design_principles/des_princ.html "Erlang OTP"
[EUnit]: http://www.erlang.org/doc/apps/eunit/chapter.html "eunit testing framework"