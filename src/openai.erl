%%
%% MIT No Attribution  
%% Copyright 2023 David J Goehrig <dave@dloh.org>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy 
%% of this software and associated documentation files (the "Software"), to 
%% deal in the Software without restriction, including without limitation the 
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
%% sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so.  
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
%% IN THE SOFTWARE.


-module(openai).
-author({ "David J Goehrig", "dave@dloh.org"}).
-copyright(<<"© 2023 David J. Goehrig"/utf8>>).
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
