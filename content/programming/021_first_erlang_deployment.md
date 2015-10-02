Title: First Erlang deployment
Date: 2015-10-03 11:31
Tags: erlang, deployment
Summary: A report on how my first Erlang deployment went down, and what I learned from it.

**TL;DR:**

* As soon as the crypto library is used check that the target machines OpenSSL
  can handle it. To be safer, use a VM with identical OS and OpenSSL to
  the target machine and build the ERTS you bundle with your release on it.
  [kerl] helps a lot here
* Always include [SASL] in your releases. The extra logging is valuable and it
  is needed for relups anyway.
* Included logging enables fast fault determination while supervisors keep the
  working parts running.
* Erlang/OTP enables quick and reliable prototyping.

First deployments in a new environment are always scary. So it was only natural
that I was afraid of the potential dangers that could have come with the
deployment of our new [Erlang] prototype. This fear was further intensified by the
fact that I was not the person who was handling the deployment. All I could do
was send a release containing a small Readme file with instructions and hope for
the best.

Technically the software was nothing special. A simple web service that
distributes data via WebWockets to all connected and subscribed clients. It
featured [cowboy] to handle the websocket part, [erlsom] to format the data that
was sent, and [mysql-otp] to talk to a database that was used to store
simulation data that needed to be replayed to simulate 'real' operation.
Our build tool of choice was [erlang.mk], which in turn uses [relx] to build
releases.

The prototype felt good. [Common Tests] covered the expected work-flow.
Key functions were tested via property testing ([triq]) and the Supervisor Tree
should at least keep everything crawling along if we did miss something.

So I ran the test suite for a last time and bundled the application together
with [SASL] before sending it to my client. It did not take long till I got
a reply. Expecting the worst I opened up the email.

The service started up, but upon a client connection things started to fall
apart. Thankfully Erlang's ecosystem had me covered and logged everything I
needed to know into the releases log directory.

```
=ERROR REPORT==== 10-Sep-2015::09:27:02 ===
Unable to load crypto library. Failed with error:
"load_failed, Failed to load NIF library
/home/<user>/<app>/lib/crypto-3.6/priv/lib/crypto: 'libcrypto.so.1.0.0:
cannot open shared object file: No such file or directory'"
OpenSSL might not be installed on this system.

=WARNING REPORT==== 10-Sep-2015::09:27:02 ===
The on_load function for module crypto returned {error, {load_failed,"Failed to
load NIF library /home/<user>/<app>/lib/crypto-3.6/priv/lib/crypto:
'libcrypto.so.1.0.0: cannot open shared object file: No such file or directory'"}}

=ERROR REPORT==== 10-Sep-2015::09:27:02 ===
Error in process <0.201.0> on node 'rsfo@127.0.0.1' with exit value:
{undef,[{crypto,hash,
    [sha, <<"EBRPiRB0U31VUD25oYQhIg==258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>], []},
    {cowboy_websocket,websocket_handshake,3,
    [{file,"src/cowboy_websocket.erl"},{line,151}]}, {cowboy_protocol,execute,4,
    [{file,"src/cowboy_protocol.erl"},{line,470}]}
]}
```

The ERTS (**E**rlang **R**un**T**ime **S**ystem) I provided with the release was
not playing nice with the OpenSSL libraries on the target machine.  To resolve
the issue I setup a VM with an identical operating system and OpenSSL library to
build a compatible ERTS version with [kerl]. Just to be sure I also enabled
dynamic-ssl-libraries for the build.

Maybe an hour after the new version was made available I got another reply
informing me that the simulation part was not working. The Supervisor tree kept
the prototype running however and my client could confirm that the messaging
part worked as expected. This was already great news. Getting them a machine
which they could use to write their clients for the service was the key goal.

The simulation could still not be started because the mysql connection could
not be established. The sql driver reported `old_server_version`. At first I
blamed the ancient MySQL version on the server,
but as it turned out it was a bug in MySQL itself
([Github Issue](https://github.com/mysql-otp/mysql-otp/issues/31)).
However, before that could be resolved via an update they moved their database
to another server version not affected by the issue and simply edited the
`sys.config` to point to the new location.

Since then we deployed some small fixes and cleanup our release process to have
less manual tasks and even enable the build of `relups`. The core systems code
however is still running today.

All in all I am quite happy on how it all went down. Especially the built in
logging capabilities and the supervision structure of OTP applications enabled
us to pinpoint faults while keeping the system partly functional even during the
earliest stages of prototyping. And the best part, those things come
automatically via OTP and don't require any extra effort that would cause
additional development time.


[erlang]: http://www.erlang.org/ "Erlang Programming Language"
[cowboy]: https://github.com/ninenines/cowboy "Small, fast, modular HTTP server written in Erlang"
[erlsom]: https://github.com/danw/erlsom "erlang libary for xml parsing"
[mysql-otp]: https://github.com/mysql-otp/mysql-otp "MySQL driver for Erlang/OTP"
[erlang.mk]: https://github.com/ninenines/erlang.mk "A build tool for Erlang that just works."
[relx]: https://github.com/erlware/relx "Sane, simple release creation for Erlang"
[Common Tests]: http://www.erlang.org/doc/apps/common_test/basics_chapter.html "Common Tests"
[triq]: https://github.com/krestenkrab/triq "Trifork QuickCheck"
[SASL]: http://www.erlang.org/doc/man/sasl_app.html "System Architecture Support Libraries"
[kerl]: https://github.com/yrashk/kerl "Easy building and installing of Erlang/OTP instances"
