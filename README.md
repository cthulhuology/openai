openai - Erlang Interface for OpenAI
=========================================



Getting Started
---------------

	openai:start(),
	openai:then( fun(JSON) -> 
		[Choices] = proplists:get_value(<<"choices">>,json:decode(JSON)),
		[Message] = proplists:get_Value(<<"message">>,Choices),
		Content = proplists:get_value(<<"content">>,Message),
		io:format("~s~n", [ Content ]) end),
	openai:chat("Write me a poem").


Design of API
-------------

This module is based on my http, json, and beamer modules.   You will need beamer
installed to make optimal use of this module. Once you have beamer installed you
can do a simple:

	beamer deps
	beamer make

And then export your OPENAI_KEY= environment variable.  To run a simple test you
can then invoke the provided escript executable:

	./openai write me a haiku

It should then proceed to write you a haiku for about a penny.


LICENSE
-------

MIT License

Copyright (c) 2024 David J Goehrig

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
