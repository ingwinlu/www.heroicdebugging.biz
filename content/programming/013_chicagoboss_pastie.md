Title: Peeking at erlang/chicagoboss
Date: 2014-07-10 14:52
Tags: erlang, chicagoboss, pastie
Summary: Developing a Paste Site in Erlang, supported by chicagoboss

###Intro

I had a lot to do with different kind of Web Frameworks these last few months, starting from the relative simple [Play Framework][Play] in Java, over [TurboGears2][TurboGears] in Python, to more complex ones like [Spring][Spring]. At first I was impressed by all of them, taking a lot of the 'hard' stuff of website development completly out of the picture. But the more time I spend with them I grew bored with all the syntactic sugar you have to add and the countless annotations you have to write. Information like routing quickly became a pain to track, let me show an example of what I mean, a code snipped of an REST Webservice in Java/Spring:

    :::Java
    @Autowired
    private CustomerService customerService;
    
    @Autowired
    private DtoToEntity dte;
    
    @Autowired
    private ReservationService reservationService;
    
    @RequestMapping(method = RequestMethod.GET, produces = "application/json")
    public List<CustomerDto> getAll() throws ServiceException {
        return EntityToDto.convertCustomers(customerService.getAllCustomer());
    }
    
    @RequestMapping(
            value = "/{id}",
            method = RequestMethod.GET,
            produces = "application/json")
    public CustomerDto getCustomerById(@PathVariable("id") Integer id)
            throws ServiceException {
        LOG.info("getCustomerById() called");
        
        if (id < 1) {
            throw new ServiceException("Invalid ID");
        }
        Customer c = customerService.getCustomer(id);
        CustomerDto ret = EntityToDto.convert(c);
        return ret;
    }
    
    @RequestMapping(
            value = "/{id}/reservations",
            method = RequestMethod.GET,
            produces = "application/json")
    public List<ReservationDto> getReservationsByCustomerId(
            @PathVariable("id") Integer id)
            throws ServiceException {
    //...
    }

So more then half of the code above is actually just notation for the framework to know how to handle the code, while the other half passes objects around to query the database, but is there a better way to handle things?


###Chicagoboss Magic

[Chicagoboss][cb] is an [Erlang][Erlang] Webframework. I toyed around with it a little bit and really liked it, for example, look at this sample controller code:


    :::erlang
    -module(erlang_server_default_controller, [Req]).
    -compile(export_all).

    index('GET', []) ->
        Counter = boss_db:count(pastie),
        {ok, [{counter, Counter}]}.

    recent('GET', []) ->
        Pasties = boss_db:find(pastie, [],[{limit,20},{order_by,paste_time},{descending,true}]),
        {ok, [{pasties, Pasties}]}.

    create('GET', []) ->
        ok;
    create('POST', []) ->
        PastieText = Req:post_param("pastie_text"),
        LanguageText = Req:post_param("language_text"),
        NewPastie = pastie:new(id, PastieText, LanguageText, erlang:now()),
        case NewPastie:save() of
            {ok, SavedPastie} ->
                {redirect, "/" ++ SavedPastie:get_pure_id()};
            {error, ErrorList} ->
                {ok, [{errors, ErrorList}, {new_pastie, NewPastie}]}
        end.

    view('GET', [PastieId]) ->
        case boss_db:find("pastie-" ++ PastieId) of
            Pastie ->
                {ok, [{pastieid, PastieId}, {foundpastie, Pastie}]};
            {error, Reason} ->
                {output, Reason}
        end.

    view_raw('GET', [PastieId]) ->
        case boss_db:find("pastie-" ++ PastieId) of
            undefined ->
                {redirect, "/", [{errors, ["blub"]}]};
            Pastie ->
                {output, Pastie:paste_string(), [{"Content-Type", "text/plain"}]};
            {error, Reason} ->
                {output, Reason}
        end.

No annotations. No Autowiring Service Layers. \o/

Mapping is done automatically by function name or via a seperate .routes file. Other really helpful features include automatically generated help pages for your database entities while starting your application in development mode and a test framework plus a lot of other things which I did not even touch myself yet.
        
###Result
You can check the result of my toying with [chicagoboss][cb] at [pastie.heroicdebugging.biz][pastie]. Remember this is run on a Rpi, still the performance seems okay. The project source is currently not available at [github][gh], though I might put it up in the future and link it here.

[Spring]: http://spring.io/  "Spring Framework"
[TurboGears]: http://turbogears.org/ "TurboGears Framework"
[Play]: http://www.playframework.com/ "Play Framework"
[Erlang]: http://www.erlang.org/ "Erlang Programming Language"
[cb]: http://www.chicagoboss.org/ "Chicagoboss MVC Framework"
[lyse]: http://learnyousomeerlang.com/ "Learn you some Erlang"
[pastie]: http://pastie.heroicdebugging.biz/ "CB powered pastie site"
[gh]: http://github.com/ "Github"
