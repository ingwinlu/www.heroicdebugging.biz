Title: XML API parsing in erlang
Date: 2014-09-23 22:47
Tags: erlang, OTP, xml, sax

Some weeks ago I wrote a application in [Erlang] which reads data from the youtube API. I wanted to parse its content efficiently via a sax parser and [xmerl] was the first search result that popped up. At first I had trouble reading its [documentation](http://www.erlang.org/documentation/doc-1/apps/xmerl/index.html) it is actually easy enough to use as long as you stay with the sax part (beware of the rest, it can kill your vm with a flood of atoms).

##Implement gen_server behaviour

The whole application is build with [Erlang OTP] in mind, so the crawler is started as a **gen_server** (via a supervisor/ppool setup) which starts to parse after it initialized itself (omitted the none functional parts):

```erlang
-module(erlytdl_crawler_worker).

-behaviour(gen_server).

-include("erlytdl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(Channel = #channel{}) ->
    %error_logger:info_msg("client crawler init~n",[]),
    {ok, Channel, 0}.

%% ...

handle_info(timeout, Channel) ->
    %% sync here, after finished, stop process
    case get_total(Channel#channel.name) of
        {ok, Total} ->
            if 
                Total > Channel#channel.total ->
                    error_logger:info_msg("~s: Parsing ~s entries~n",
                        [Channel#channel.name, integer_to_list(Total-Channel#channel.total)]),
                    get_new_entries(
                        build_url(
                            channel_info,
                            [Channel#channel.name, integer_to_list(1), integer_to_list(25)]),
                        Total-Channel#channel.total,
                        Channel),
                    update_total(Channel, Total),
                    %% update total in channel
                    {stop, normal, Channel};
                true ->
                    {stop, {shutdown, nothing_new}, Channel}
            end;
        Error ->
            {stop, {shutdown,Error}, Channel}
    end;
handle_info(Msg,Channel) ->
    error_logger:error_msg("Unknown Message: ~p~n",[Msg]),
    {noreply,Channel}.
%% ...
```

##Making the HTTPC request

Before we can parse the xml content we need to query it from the server via httpc via **get_new_entries**:

| **Argument**  | **Description **                   |
| ------------- | ---------------------------------- |
| URL      | The URL we need to query       |
| ToFind   | How many more entries we need to find (gets calculated via parsing of total entries and how many entries we already found) |
| Channel  | General information about the channel we are parsing, needed for DB interaction (not covered here) |

```erlang
get_new_entries(none, _ToFind, _Channel) ->
    finished;
get_new_entries(_URL, ToFind, _Channel) when ToFind =< 0 ->
    finished_early;
get_new_entries(URL, ToFind, Channel) when ToFind > 0 ->
    case httpc:request(URL) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, #parser_state{file_list=FileList, next_url=NextUrl}, _} = parse_xml(Body),
            NewToFind = insert_files(FileList, ToFind, Channel),
            get_new_entries(NextUrl, NewToFind, Channel);
        {ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}} ->
            error_logger:error_msg("404 during httpc:request(~s)~n", [URL]),
            not_found
    end.
```

##xmerl sax parser in action
Now that the basics are out of the way we can get to the real thing, parsing the xml content via **parse_xml**, which gets passed the xml content we retrieved via the httpc:request from earlier. The most crucial part of sax parsing is to have a decent data structure to put your data into, we are going to use **parser_state** for that purpose.

```erlang
-record(parser_state,{status=intro, last_string="", file_list=[], next_url=none}).
```

*   `status`: initialized with `intro` and keeps track of the 'mode' the parser is currently in
*   `last_string`: will keep track of the last string the parser found, we need this to access the content of xml entities
*   `file_list`: finished list of entries we parsed, we are going to use this as an accumulator
*   `next_url`: youtube's API tells us where we need to query next to get more entries, so instead of calculating it our selfs, we are going to save this

```erlang
parse_xml(XML) ->
    xmerl_sax_parser:stream(XML, [
        {event_fun, 
            fun
                ({startElement, _,"entry", _, _}, _, State = #parser_state{file_list=Files}) ->
                    State#parser_state{status=ready, file_list=[#file{}|Files]};
                ({startElement, _,"link", _, 
                        [{[],[],"rel","next"},
                        {[],[],"type","application/atom+xml"},
                        {[],[],"href",NextUrl}]},
                        _, State = #parser_state{}) ->
                    State#parser_state{next_url=NextUrl};
                (_, _, State = #parser_state{status=intro}) ->
                    State; % ignore everything while not in entry yet
                ({startElement, _,"link", _, 
                        [{[],[],"rel","alternate"},
                        {[],[],"type","text/html"},
                        {[],[],"href",VideoUrl}]},
                        _, State = #parser_state{file_list=[Current = #file{}|Other]}) ->
                    State#parser_state{file_list = [Current#file{video_url=VideoUrl}|Other]};
                ({endElement, _, "published", _}, _, 
                        State = #parser_state{file_list=[Current = #file{}|Other], last_string=Str}) ->
                    State#parser_state{file_list=[Current#file{timestamp=Str}|Other]};
                ({endElement, _, "title", _}, _, 
                        State = #parser_state{file_list=[Current = #file{}|Other], last_string=Str}) ->
                    State#parser_state{file_list=[Current#file{title=Str}|Other]};
                ({characters, Str}, _, State = #parser_state{}) ->
                    State#parser_state{last_string=Str};
                (_, _, State) ->
                    State
            end},
        {event_state, #parser_state{}}
        ]).
```

We pass our initial state to the sax parser via the `{event_state, #parser_state{}}` option. The other option we pass is our `event_fun` which gets called by the parser for every [event](http://www.erlang.org/documentation/doc-1/man/xmerl_sax_parser.html) that occurs. We are mostly going to use `startElement` and `endElement` here, which are called on start/end tags. In case the parser finds a string we save it in `#parser_state.last_string` so we can use it in the next `endElement` that occurs. Every other State gets caught by the last pattern we match against `(_, _, State)` which simply returns an unmodified `State`. Also note the third pattern we match against where we catch events that don't match the first two while we are in the `intro` state.

Thats all there is to it, we end up with a `Result` tuple which, if parsing succeeds, has the form of `{ok, #parser_state{file_list=FileList, next_url=NextUrl}, _Rest}`. We take the populated `FileList` as well as the `NextUrl` from the last State and repeat the whole process if necessary.

The full [source](https://github.com/ingwinlu/erlytdl/blob/master/src/erlytdl_crawler_worker.erl) is available on github.

[xmerl]: http://www.erlang.org/documentation/doc-1/apps/xmerl/index.html "xmerl Documentation"
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
