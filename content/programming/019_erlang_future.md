Title: Asynchronous function calling in Erlang
Date: 2015-01-19 10:47
Tags: erlang, erlfu
Summary: A look at asynchronous functions in Erlang and why there is no real need for a support library.

#intro
Thanks to `nox` from freenode/#erlang i stumbled about [erlfu] today.
First I thought that futures (for example: [python-futures]) in erlang makes sense, but now I am not so sure anymore.

Why did I never have the need for such libraries?
Either my use-case or environment was too complex, so I was most likely in a supervisor tree.
Here I can spawn my tasks via a simple one for one supervisors and don't really have to think about what happens to my processes and get answers whenever.

The other possibility is that my calls are simple, then i can just use one of the following possibilities.

#simple implementations
##simple async function
To implement a simple asynchronous action you can just use `fun` and `spawn/1`:

    :::erlang
    spawn(fun() -> timer:sleep(2000), boom end).

##callable sync function
###via messages
If we want to read a return value it gets a bit trickier.
We can use Erlang's Messages to get it though:

    :::Erlang
    -module(simple_future).
    -export([bomb/1]).    
    
    bomb(Pid) ->
        timer:sleep(2000),
        Pid ! {self(), boom}.
    
    29>  spawn(simple_future, bomb, [self()]).
    <0.83.0>
    30> flush().
    ok
    31> flush().
    Shell got {<0.83.0>,boom}
    ok

###via trap_exit
Another approach would be to trap exits of your spawned processes:

    :::Erlang
    17> process_flag(trap_exit, true).
    false
    18> spawn_link(fun() -> timer:sleep(2000), exit(boom) end).
    <0.66.0>
    19> flush().
    Shell got {'EXIT',<0.66.0>,boom}

##rpc:async_call
Using [rpc][erlang-rpc] is surely the most sophisticated approach.
`async_call/4` is exactly what we want:

    :::Erlang
    -module(simple_future).
    -export([rpcbomb/0]).
    
    rpcbomb() ->
        timer:sleep(2000),
        boom.
    
    10> Key = rpc:async_call(node(), simple_future, rpcbomb, []).
    <0.57.0>
    11> rpc:nb_yield(Key, 5000).
    {value,boom}

#conclusion
Not really sure where I wanted to go with this post.

On one hand I wanted to highlight how great Erlang is at this kind of stuff.
You never really wish for a library to do such tasks because it is intuitively possible without a lot of trouble.

On the other hand I don't want to bash [erlfu].
It probably has it's uses though I can not come up with any that [rpc][erlang-rpc] could not handle.


[erlfu]: https://github.com/gleber/erlfu "erlfu on github"
[python-futures]: https://docs.python.org/3/library/concurrent.futures.html "python-futures documentation"
[erlang-rpc]: http://erlang.org/doc/man/rpc.html "erlang rpc documentation"