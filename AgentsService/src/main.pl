/*-------------------------------------------
Author:		James King adapted from Anne Ogborn
Title:		main.pl
Created:	4th Nov 2018
Desc:		Contains the central code and handlers for the Agents web service
-------------------------------------------*/

% Compile and set up mvfcec
?- ['./mvfcec/src/lib/utilities'].
?- ['./mvfcec/src/compiler/basic_V1.0'].
?- ['./mvfcec/src/lib/activity_recognition_lifecycles'].
dialect(swi).
:- dynamic observed_at/2.
input_format(observed_at(E, T), E, T).

% The libraries required for a server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).

% The agents system logic
?- ['./strategies'].
?- ['./communities'].
?- ['./percepts'].
?- ['./agents'].
?- ['./check_beliefs'].
?- ['./revise'].


% Set correct handling of JSON
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- multifile http_json/1.

http_json:json_type('application/x-javascript').
http_json:json_type('text/javascript').
http_json:json_type('text/x-javascript').
http_json:json_type('text/x-json').

% Divide the paths up into sections
:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(create, '/create', []).
http:location(get_data, '/get', []).

% The handlers for different routes
:- http_handler(get_data(strategies), get_strategies, []).
:- http_handler(root(get_action), get_action, []).
:- http_handler(root(add_percept), add_percept, []).
:- http_handler(root(check_belief), check_belief, []).
:- http_handler(create(new_agent), create_new_agent, []).
:- http_handler(create(new_community), create_new_community, []).
:- http_handler(create(new_generation), create_new_generation, []).


% Starts the server on the port number passed in Port
% Creates a number of threads and returns to the top level
server(Port):-
	http_server(http_dispatch, [port(Port)]).

% Handler that finds all the strategies in the system and replies with a json
get_strategies(Request):-
	member(method(get), Request), !,
	find_strategies(Strategies),
	reply_json_dict(strategies{strategies: Strategies}).

% Handle a request to get an action from an agent
get_action(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	agent_action(DictIn, Action),
	reply_json(return{action: Action}).

% Handles a request to create a new agent in the knowledge base
create_new_community(Request):-
	member(method(get), Request), !,
	new_community(ID),
	reply_json(return{id: ID}).
	
% Handles a request to create a new agent in the knowledge base
create_new_agent(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	new_agent(DictIn, Status),
	reply_json(return{status: Status}).

% Handles a request to create a new generation in the logic base
create_new_generation(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	new_generation(DictIn, Status),
	reply_json(return{status: Status}).

% Handles a request to add a new percept to an agent
add_percept(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_percept(DictIn, Status),
	reply_json(return{status: Status}).

% Handles a request to check the value of a beliefs
check_belief(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	get_belief(DictIn, Value),
	reply_json(return{value: Value}).