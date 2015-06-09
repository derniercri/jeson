-module(json).
-export([gen_decoder/3, gen_encoder/2]).
-include("json_type.hrl").
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-vsn(1).



%% @doc generate a function which convert a json string into an erlang record. 
%% @spec gen_decoder(Type_list:: [json_type()], Fields::[string()], Record_name::atom()) -> fun ((string()) -> tuple())
gen_decoder(Type_list, Field_names, Record_name) -> 
    json_decoder:gen(Type_list, Field_names, Record_name).



%% @doc generate a fonction which convert erlang value into a json string
%% @spec gen_decoder(Type_list:: [json_type()], Fields::[string()]) -> fun (tuple() -> (string()))
gen_encoder(Type_list, Field_names) ->
    json_encoder:gen(Type_list, Field_names).
