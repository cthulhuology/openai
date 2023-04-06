
-module(openai).
-behavior(gen_server).
-export([ start_link/0, models/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, engines/0, engine/1 ]).

start_link() ->
	gen_server:start_link({ local, ?MODULE}, ?MODULE, { }, []).
	
models() ->
	gen_server:cast(?MODULE,models).

engines() ->
	gen_server:cast(?MODULE,engines).

engine(Id) ->
	gen_server:cast(?MODULE,{engine, Id }).


init({ }) ->
	{ ok, []}.

handle_call(Message, _From, State) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(models, State) ->
	http:then( fun(Json) -> ?MODULE ! { models, json:decode(Json) } end),
	http:get("https://api.openai.com/v1/models", [ 'Authorization' ]),
	{ noreply, State };

handle_cast(engines, State) ->
	http:then( fun(Json) -> ?MODULE ! { engines, json:decode(Json) } end),
	http:get("https://api.openai.com/v1/engines", [ 'Authorization' ]),
	{ noreply, State };

handle_cast({engine,Id}, State) ->
	http:then( fun(Json) -> ?MODULE ! { engine, Id, json:decode(Json) } end),
	http:get("https://api.openai.com/v1/engines/" ++ Id, [ 'Authorization' ]),
	{ noreply, State };

handle_cast(Message, State) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info({models, Models}, State) ->
	io:format("Models ~p~n", [ Models ]),
	{ noreply, State };

handle_info({engines, Engines}, State) ->
	io:format("Engines ~p~n", [ Engines ]),
	{ noreply, State };

handle_info({engine, Id, Engine}, State) ->
	io:format("Engine ~p >> ~p~n", [ Id, Engine ]),
	{ noreply, State };


handle_info(Message, State) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

terminate( _Reason, _State) ->
	ok.

code_change( _Old, State, _Extra ) ->
	{ ok, State }.
