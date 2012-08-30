%% ==============================================================================
%
% HL72XML PARSER
%
% Copyright (c) 2012 Nestor Ocampo <anezt_oh@hotmail.com>.
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
% 3. Neither the name of copyright holders nor the names of its
%    contributors may be used to endorse or promote products derived
%    from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%% ===============================================================================


-module(hl72xml_parser).

-vsn("0.1").

-include("hl72xml.hrl").

-export([start/1]). 

%% =================================================================
%% Public API
%% =================================================================

%% @spec start(Msg :: string()) -> string()
%% @doc Retrieve a decoded hl7 message format into xml format.
-spec start(Msg :: string()) -> string().
start(Msg)->
    Res = {segments,parse(?SPLIT(Msg,?DEL_SEGMENT),?TYPES)},
    hl72xml_xml:encode(Res).


%% =================================================================
%% Private API
%% =================================================================

%% @spec parse([] | list(), list() | {match, list()} | {nomatch, list()}) -> list()
%% @doc Parse a hl7 message into a proplist.
-spec parse([] | list(), list() | {match, list()} | {nomatch, list()}) -> list().
parse([],_)                                        -> [];
parse([H|T],X=["segments"|Y])                      ->
    [{segment,  parse(?SPLIT(H,?DEL_FIELD),Y)}] ++ parse(T,X);
parse([H|T],["header"|Y])                          ->
    [{H,[{fields, parse(T,Y) }]}];
parse([?ENCODING|T],X=["fields"|_])                ->
    [{field,[{value,?ENCODING}]}] ++ parse(T,X);
parse([H|T],X=["fields"|Y])                        ->
    [{field,parse(?SPLIT(H,?DEL_REPEAT_FIELD), 
		  {match(H, ?DEL_REPEAT_FIELD), Y})}]
	++ parse(T,X);
parse([H|T], {match, X=["components"| Y]})         ->
    [{value, parse(?SPLIT(H,?DEL_COMPONENT),
		   {match(H, ?DEL_COMPONENT), Y}) }]
	++parse(T, {match, X});
parse([H|_], {nomatch, ["components"| Y]})         ->
    [{value, parse(?SPLIT(H,?DEL_COMPONENT),
		   {match(H, ?DEL_COMPONENT), Y}) }];
parse(List,{match, ["subcomponents"| Y]})        ->
    [{components, [{component,
		    parse(?SPLIT(H, ?DEL_SUBCOMPONENT),
			  {match(H, ?DEL_SUBCOMPONENT), Y}) } || H <- List  ] }];
parse([H|_],{ nomatch,  ["subcomponents"| Y]} ) -> 
    parse(?SPLIT(H, ?DEL_SUBCOMPONENT), {match(H, ?DEL_SUBCOMPONENT), Y});
parse(X, {match, _})                               ->
    [{subcomponents, [{subcomponent, SC} || SC <- X]}];
parse([H|_], {nomatch, _})                         -> H.


%% @spec match(String :: string(), RegExp :: string()) -> match | nomatch
%% @doc If the expression match or not with the regexp given.
-spec match(String :: string(), RegExp :: string()) -> match | nomatch.
match(String, RegExp)->
    case re:run(String, RegExp) of
        {match, _} -> 
	    match;
        X        ->
	    X
    end.
