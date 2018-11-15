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
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).

% My modules 
:- use_module(strategies).
:- use_module(communities).

% Set correct handling of JSON
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- multifile http_json/1.

http_json:json_type('application/x-javascript').
http_json:json_type('text/javascript').
http_json:json_type('text/x-javascript').
http_json:json_type('text/x-json').

% The handlers for different routes
:- http_handler(root(get_strategies), get_strategies, []).
:- http_handler(root(get_action), get_action, []).
:- http_handler(root(new_agent), new_agent, []).
:- http_handler(root(new_community), new_community, []).
:- http_handler(root(new_generation), new_generation, []).


% Starts the server on the port number passed in Port
% Creates a number of threads and returns to the top level
server(Port):-
	http_server(http_dispatch, [port(Port)]).

% Handler that finds all the strategies in the system and replies with a json
get_strategies(Request):-
	member(method(get), Request), !,
	strategies:find_strategies(Strategies),
	reply_json_dict(strategies{strategies: Strategies}).

% Handle a request to get an action from an agent
get_action(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	strategies:agent_action(DictIn, Action, Status),
	( Status == "Good" -> reply_json(return{action: Action, status: Status}) ; reply_json(return{status: Status}) ).

% Handles a request to create a new agent in the knowledge base
new_community(Request):-
	member(method(get), Request), !,
	communities:new_community(ID),
	reply_json(return{id: ID}).
	
% Handles a request to create a new agent in the knowledge base
new_agent(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	strategies:new_agent(DictIn, Status),
	reply_json(return{status: Status}).

% Handles a request to create a new generation in the logic base
new_generation(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	communities:new_generation(DictIn, Status),
	reply_json(return{status: Status}).
