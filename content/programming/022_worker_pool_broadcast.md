Title: Broadcasting to wpool workers
Date: 2016-02-06 16:57
Tags: erlang, wpool

[wpool] is my preferred [erlang] worker pool library. But there is
no easy way to broadcast a message to all spawned workers. Or is there?

In this article we will have a look at [wpool], how to easily replace a single
`gen_server` with a process pool and finally how to utilize process groups to
broadcast messages to all the workers in a `wpool`.

## wpool
The thing that makes [wpool] great is how it interfaces with your existing
code. You notice that messages to a `gen_server` are building up and are not
processed quickly enough. Changing a few calls from `gen_server` to
`wpool` is trivial and you have a fully operational worker pool to handle the
task.

## Example
The code for the example can be found at this [repository][example repo].
Each commit coincides with one of the steps below. The
[initial commit](https://github.com/ingwinlu/wpool_broadcast_example/commit/8c1fb84876bc8d507d93ae7bb9b83149a24a5a66)
sets up the project with [erlang.mk].

If we look at the [second commit](<https://github.com/ingwinlu/wpool_broadcast_example/commit/06e2b6dff4ce8ba631a0cbf9d3c4bbdc8e8c6151)
we can see the simple `gen_server` that we will use in this example. It wraps
around a Counter that increases each time we call our `bump` function.

```
2> {ok, Pid} = broadcast:start_link().
{ok,<0.37.0>}
3> broadcast:bump(Pid).
ok
4> 
=INFO REPORT==== 6-Feb-2016::15:09:24 ===
Counter of <0.37.0> is at 1

4> broadcast:bump(Pid).

=INFO REPORT==== 6-Feb-2016::15:09:27 ===
Counter of <0.37.0> is at 2
ok
```

So far so good. Now let's make good on our promise to show how easy it is to
wrap that in a worker pool with [wpool].

[The next commit](https://github.com/ingwinlu/wpool_broadcast_example/commit/048b996c6d63e750aa55bd7e5ea441654b554e2d)
changes the calls from `gen_server` to `wpool` and adds it as a project
dependency so it gets downloaded and compiled by [erlang.mk].

```
1> application:ensure_all_started(worker_pool).
{ok,[worker_pool]}

2> broadcast:start_pool().
{ok,<0.41.0>}
3> broadcast:bump().

=INFO REPORT==== 6-Feb-2016::15:26:32 ===
Counter of <0.44.0> is at 1
ok
4> broadcast:bump().

=INFO REPORT==== 6-Feb-2016::15:26:33 ===
Counter of <0.44.0> is at 2
ok
5> broadcast:bump().

=INFO REPORT==== 6-Feb-2016::15:26:34 ===
Counter of <0.44.0> is at 3
ok
6> [broadcast:bump() || X <- lists:seq(1,5)].

=INFO REPORT==== 6-Feb-2016::15:27:11 ===
Counter of <0.44.0> is at 4

=INFO REPORT==== 6-Feb-2016::15:27:11 ===
Counter of <0.53.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:27:11 ===
Counter of <0.45.0> is at 1
 
=INFO REPORT==== 6-Feb-2016::15:27:11 ===
Counter of <0.46.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:27:11 ===
Counter of <0.47.0> is at 1
```

Well that was easy; but how can we now send a message reliably to all workers
in that pool as stated in the opening of the article?

The answer is simple. We [put all of our workers into a process
group](https://github.com/ingwinlu/wpool_broadcast_example/commit/192b8bd53a1645adc3f2d2537b8a4e5b47e90a41).
For this example we choose [pg2], which is shipped with [erlang]. But notice
that there are a lot of different process group libraries out there that might
fit your use case better.

Now we have a way of accessing all our worker pids as well as an API function
to send all of them a message.

```
1> application:ensure_all_started(worker_pool).
{ok,[worker_pool]}

=INFO REPORT==== 6-Feb-2016::15:37:01 ===
Creating wpool ETS table2> 
2> l(broadcast).
{module,broadcast}
3> broadcast:start_pool().
{ok,<0.43.0>}
4> broadcast:bump().

=INFO REPORT==== 6-Feb-2016::15:37:21 ===
Counter of <0.46.0> is at 1
ok
5> broadcast:bump_all().

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.46.0> is at 2

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.47.0> is at 1
<0.58.0>

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.48.0> is at 1
6> 
=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.49.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.50.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.51.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.52.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.53.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.54.0> is at 1

=INFO REPORT==== 6-Feb-2016::15:37:27 ===
Counter of <0.55.0> is at 1
```

[erlang]: http://www.erlang.org/ "Erlang Programming Language"
[erlang.mk]: http://erlang.mk/ "A build tool for Erlang that just works"
[example repo]: https://github.com/ingwinlu/wpool_broadcast_example "wpool broadcast example repository"
[wpool]: https://github.com/inaka/worker_pool "Erlang worker pool"
[pg2]: http://erlang.org/doc/man/pg2.html "Distributed Named Process Groups"
