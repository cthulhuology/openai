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
-copyright(<<"© 2024 David J. Goehrig"/utf8>>).
-export([ authorization/0, start/0,
	models/0, engines/0, engine/1, 
	chat/1, then/1, message/1 ]).

-define(ENDPOINT, "https://api.openai.com/v1").

authorization() ->
	Auth = list_to_binary(os:getenv("OPENAI_KEY")),
	<<"Bearer ", Auth/binary>>.

message(JSON) ->
	[Choices] = proplists:get_value(<<"choices">>,json:decode(JSON)),
	Message = proplists:get_value(<<"message">>,Choices),
	proplists:get_value(<<"content">>,Message).

start() ->
	Self = self(),
	HTTP = http:start(),
	http:then(fun(JSON) -> Self ! JSON end),
	HTTP.

then(Fun) ->
	http:then(Fun).
	
models() ->
	http:get(?ENDPOINT ++ "/models", [ { <<"Authorization">>, authorization() } ]).

engines() ->
	http:get(?ENDPOINT ++ "/engines", [ { <<"Authorization">>, authorization() } ]).

engine(Id) ->
	http:get(?ENDPOINT ++ "/engines/" ++ Id, [ { <<"Authorization">>, authorization() } ]).

chat(Prompt) when is_list(Prompt) ->
	chat(list_to_binary(Prompt));
chat(Prompt) ->
	Payload = json:encode([
		{ <<"model">>, <<"gpt-4o-mini">>}, 
		{ <<"messages">>, [ [{<<"role">>, <<"user">>}, {<<"content">>, Prompt }]]}]),
	http:post(?ENDPOINT ++ "/chat/completions", [
		{ <<"content-type">>, <<"application/json">> },
		{ <<"content-length">>, integer_to_binary(byte_size(Payload)) }, 
		{ <<"Authorization">>, authorization() } ], Payload).
