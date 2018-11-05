/*-------------------------------------------
Author:		James King adapted from Anne Ogborn
Title:		main.pl
Created:	4th Nov 2018
Desc:		Contains the central code and handlers for the Agents web service
-------------------------------------------*/

% The libraries required for a server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).

% My modules 
:- use_module(strategies).

% Set correct handling of JSON
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- multifile http_json/1.

http_json:json_type('application/x-javascript').
http_json:json_type('text/javascript').
http_json:json_type('text/x-javascript').
http_json:json_type('text/x-json').

% The handlers for different roots
:- http_handler(root(get_strategies), get_strategies, []).

% Starts the server on the port number passed in Port
% Creates a number of threads and returns to the top level
server(Port):-
	http_server(http_dispatch, [port(Port)]).

% Handler that finds all the strategies in the system and replies with a json
get_strategies(Request):-
	member(method(get), Request), !,
	strategies:find_strategies(Strategies),
	reply_json_dict(strategies{strategies: Strategies}).

