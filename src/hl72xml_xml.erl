%% ==============================================================================
%
% HL72XML XML
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


-module(hl72xml_xml). 

-vsn("0.1").

-export([encode/1]). 


%% =================================================================
%% Public API
%% =================================================================

%% @spec encode(tuple() | list()) -> string()
%% @doc Encoding a proplists into xml format.
-spec encode(tuple() | list()) -> string().
encode({Node, Params, Elements}) ->
    NodeStr = to_str(Node),
    "<"++NodeStr++" "++Params++">"++encode(Elements)++"</"++NodeStr++">";
encode({Node,Elements}) ->
    NodeStr = to_str(Node),
    "<"++NodeStr++">"++encode(Elements)++"</"++NodeStr++">";
encode([]) -> "";
encode([{Key,Value}|T]) ->
    {KeyStr,ValStr} = {to_str(Key),to_str(Value)},
    "<"++KeyStr++">"++encode(ValStr)++"</"++KeyStr++">"++
	encode(T);
encode(Single) ->
    SingleStr = to_str(Single),
    SingleStr.

%% =================================================================
%% Private API
%% =================================================================

%% @spec to_str(list() | atom() | float() | integer()) -> string()
%% @doc Converts a erlang term to string term.
-spec to_str(list() | atom() | float() | integer()) -> string().
to_str(X) when is_list(X) -> X;
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_float(X) ->
    lists:flatten(io_lib:format("~.2f", [X]));
to_str(X) when is_integer(X) -> integer_to_list(X).


