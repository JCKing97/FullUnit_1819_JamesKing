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
?- ['./beliefs'].
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
http:location(community, root(community), []).
http:location(strategies, root(strategies), []).
http:location(generation, root(generation), []).
http:location(agent, root(agent), []).
http:location(percept, root(percept), []).
http:location(action, root(action), []).
http:location(percept_action, percept(action), []).


% The handlers for different routes
:- http_handler(root(strategies), strategies, []).
:- http_handler(root(community), community, []).
:- http_handler(root(generation), generation, []).
:- http_handler(root(agent), agent, []).
:- http_handler(percept_action(interaction), percept_action_interaction, []).
:- http_handler(percept_action(gossip), percept_action_gossip, []).
:- http_handler(percept(interaction), percept_interaction, []).
:- http_handler(root(action), action, []).


% Starts the server on the port number passed in Port
% Creates a number of threads and returns to the top level
server(Port):-
	http_server(http_dispatch, [port(Port)]).

% Handler that finds all the strategies in the system and replies with a json
strategies(Request):-
	member(method(get), Request), !,
	find_strategies(Strategies),
	reply_json_dict(strategies{success: true, status: 200, strategies: Strategies}).

% Handle a request to get an action from an agent
get_action(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	agent_action(DictIn, Action),
	reply_json(return{action: Action}).

% Handles a request to create a new agent in the knowledge base
community(Request):-
	member(method(put), Request), !,
	new_community(ID),
	reply_json(return{success: true, status: 200, id: ID}).
	
% Handles a request to create a new agent in the knowledge base
agent(Request):-
	member(method(put), Request), !,
	http_read_json_dict(Request, DictIn),
	new_agent(DictIn, Success),
	reply_json(return{data: DictIn, success: Success, status: 200}).

% Handles a request to create a new generation in the logic base
generation(Request):-
	http_log('Generation', []),
	member(method(put), Request), !,
	http_log('DictIn', []),
	http_read_json_dict(Request, DictIn),
	http_log('Generation3', []),
	new_generation(DictIn, Success),
	http_log('Success: ~w~n',[Success]),
	reply_json(return{data: DictIn, success: Success, status: 200}).

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