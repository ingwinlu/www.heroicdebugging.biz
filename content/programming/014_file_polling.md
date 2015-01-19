Title: Writing an Erlang OTP File Poller
Date: 2014-08-27 22:47
Tags: erlang, OTP
Summary: A short introduction to Erlang OTP and how to program a File Poller which implements gen_server

###Intro
I have been playing with the idea to write a content management software for some time now. Reading files and pre-generating pages, but still be able to serve dynamic content. While having no Idea how the big players (joomla, wordpress, ...) do it I thought it would be a good lesson on the problems such systems face.

As a first step I wanted to simply serve some Markdown files dynamically (i.e. changes and additions should be reloaded instantly), providing features similar to what [pelican] offers (which powers this blog).

Since [Erlang] is my current poison it only made sense to use it and [Chicago Boss] to build it but first, I needed to build a library to poll for files.

###Erlang OTP - gen_server
[Erlang OTP] is a collection of design principles on how to implement common parts of processes. Erlang goes ahead and calls these abstractions **Behaviours**, in our case that would be a Server-Client behaviour, called **gen_server**. The idea is to divide the program into a generic part and a specific part, which gets executed via callbacks from the generic one.

Our File Polling Server, after being started, should go ahead and periodically (via a asynchronous call to itself) check if files have been modified lately. A client can then pull a list of files which have been found. Let's see how it does that!

###Code and Explanation
    :::Erlang
    -module(scanner).
    -behaviour(gen_server).
    -include_lib("kernel/include/file.hrl"). % For FileInfo record used later
    -record(state,{directory, regex, timeout=1000, files=[], poll=0}).
    
    -export([start_link/3,pull/1,stop/1]).
    -export([init/1,handle_call/3,handle_cast/2,
        handle_info/2,code_change/3,terminate/2]).

The header of the file is fairly standard for an Erlang program. First we define the name of the *-module*, include a *-behaviour* specification (which is somewhat familiar to interfaces in other languages) and follow it up with an *-include_lib* to use the **FileInfo** record as well as a definition of our **state** *-record*, which will keep track of our variables.

Then we export the public accessible functions for our module, splitting it up allows for easier reading. The first export handles all functions which should be used by clients, while the second one is used for **gen_server** callbacks.

    :::Erlang
    %%%Client API
    %%Starts Server
    start_link(Directory, Regex, Timeout) ->
        gen_server:start_link(?MODULE, [Directory, Regex, Timeout], []).

    %%Pull List of Files that changed
    pull(Pid) ->
        gen_server:call(Pid, pull).
    %%Stop Server
    stop(Pid) ->
        gen_server:call(Pid, terminate).

This is the part which a client is supposed to interact with. **scanner:start_link(Directory, Regex, Timeout)** returns {ok,Pid} if successfully started, the Pid will be used by a Client to interact to the Server. For example **scanner:pull(Pid)** would pull a List of changed Files to the Client. Stop does exactly what is expected and terminates the server.

    :::Erlang
    %%%Server
    init([Directory, Regex, Timeout]) ->
        timer:send_after(0,scan),
        {ok, #state{directory=Directory, regex=Regex, timeout=Timeout, files=[], poll=0}}.

Now for the definition of functions which will be called by **gen_server**. Init will be called by start_link, sending the token scan towards itself (i.e.: self() ! scan) after 0 seconds, afterwards we build up our state record with the starting parameters and default values for the rest of them.

    :::Erlang
    handle_call(pull,_From, State) ->
        {reply,State#state.files,State#state{files=[]}};
    handle_call(terminate, _From, State) ->
        {stop, normal, ok, State}.

handle_call is responsible for synchronous messages, we use two of them. The first one sends the current contents of our *state.files* to the requesting client and sets an empty list into the record. The second one terminates the Server.

    :::Erlang
    handle_cast(_, State) ->
        {noreply, State}.

handle_cast is for asynchronous messaging, which we don't use. In future iterations we could implement changing of the watched directory or regular expression here.

    :::Erlang
    handle_info(scan, State) ->
        {New_files,New_Poll} = get_new_files(
            State#state.directory,
            State#state.regex, 
            State#state.poll)
        ,
        MergedFiles = lists:usort(New_files ++ State#state.files),
        timer:send_after(State#state.timeout,self(),scan),
        {noreply,State#state{files=MergedFiles,poll=New_Poll}};
    handle_info(Msg,State) ->
        error_logger:error_msg("Unknown Message: ~p~n",[Msg]),
        {noreply,State}.

Now it gets really interesting! As mentioned above we use a timer to send a scan message to our-selfs after every *Timeout*. This triggers handle_info which is called by gen_server for every message that is not a synchronous or asynchronous request. This is where we check for new files via **get_new_files** (which we will talk about later). The results are merged with the existing file list, usorted (unique sort) and then a timer is setup, which will repeat the whole process in *state.timeout* milliseconds.

The other guard is just for debugging and catches other messages (and prints them via *error_logger*).

    :::Erlang
    code_change(_OldVsn, Clients, _Extra) ->
        {ok, Clients}.

    terminate(normal, State) ->
        error_logger:info_msg("Last state: ~w~n",[State]),
        ok.

code_change is a feature for hot code reloading, which I did not touch yet, while as terminate is a function called gen_server wants to shut down. In out case it prints the current *state* record and quits.

    :::Erlang
    %%%Private
    get_new_files(Directory, Regex, LastPollTime) ->
        NewPollTime = {date(), time()},
        NewFiles = filelib:fold_files(
            Directory, 
            Regex, 
            true, 
            fun(File, Acc) -> 
                {ok, FileInfo} = file:read_file_info(File),
                FileMtime = FileInfo#file_info.mtime,
                if 
                    FileMtime>LastPollTime ->
                        [File|Acc];
                    true ->
                        Acc
                end
            end,
            []
        ),
        { NewFiles, NewPollTime }.

Here is the private part of the Poller, which actually handles the logic of getting new files. I borrowed the idea from [this blog post](http://aleph-nought.blogspot.co.at/2010/09/more-erlang-building-file-poller.html), though I have rewritten it somewhat. First we check the files mtime versus the current poll time. All this can be put into a single fold_files call, which we use to recursively (thats the *true* part in the call) search for all files matching *Regex* in *Directory* and add it to a list via an anonymous function. There we also test the files mtime and add the File to our NewFiles List if it is indeed newer.

The full [source can be found here](|filename|/static/scanner.erl).
###Usage
This part is fairly simple, since we abstracted that nicely! 

    :::Erlang
    [winlu@micronuke lib]$ erl
    Erlang/OTP 17 [erts-6.1] [source] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V6.1  (abort with ^G)
    1> c(scanner).
    {ok,scanner}
    2> {ok, Pid} = scanner:start_link(".",".*",5000).
    {ok,<0.44.0>}
    3> scanner:pull(Pid).
    ["./scanner.beam","./reader_md.erl","./scanner.erl"]
    4> scanner:pull(Pid).
    []
    5> scanner:pull(Pid).
    ["./New Text Document.txt"]
    6> scanner:stop(Pid).

    =INFO REPORT==== 27-Aug-2014::22:20:19 ===
    Last state: {state,[46],[46,42],5000,[],63576397219}
    ok
    15>

###Whats next
This implementation does not keep track of file deletion, so it would be a nice idea to actually have two lists of files in the state, as well as **add, modify, delete** responses. To stay inline with OTP we could spawn processes for every file we find and use those to send us a message if a statechange occurs.

###Conclusion
I had a lot of fun playing around with Erlang today and hope this fact carried over in this post as well, also shoutout to the fine people in #erlang on freenode, who looked over my code. 


[Spring]: http://spring.io/  "Spring Framework"
[TurboGears]: http://turbogears.org/ "TurboGears Framework"
[Play]: http://www.playframework.com/ "Play Framework"
[Erlang]: http://www.erlang.org/ "Erlang Programming Language"
[Chicago Boss]: http://www.chicagoboss.org/ "Chicagoboss MVC Framework"
[lyse]: http://learnyousomeerlang.com/ "Learn you some Erlang"
[pastie]: http://pastie.heroicdebugging.biz/ "Chicago Boss powered pastie site"
[gh]: http://github.com/ "Github"
[pelican]: http://docs.getpelican.com/en/latest/ "Pelican"
[Erlang OTP]: http://www.erlang.org/doc/design_principles/des_princ.html "Erlang OTP"
