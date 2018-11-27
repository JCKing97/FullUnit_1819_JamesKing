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
?- ['./actions'].


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
http:location(belief, root(belief), []).


% The handlers for different routes
:- http_handler(root(strategies), strategies, []).
:- http_handler(root(community), community, []).
:- http_handler(root(generation), generation, []).
:- http_handler(root(agent), agent, []).
:- http_handler(percept_action(interaction), percept_action_interaction, []).
:- http_handler(percept_action(gossip), percept_action_gossip, []).
:- http_handler(percept(interaction), percept_interaction, []).
:- http_handler(root(action), action, []).
:- http_handler(belief(donor), belief_donor, []).
:- http_handler(belief(recipient), belief_recipient, []).
:- http_handler(belief(interaction), belief_interaction, []).
:- http_handler(belief(standing), belief_standing, []).


% Starts the server on the port number passed in Port
% Creates a number of threads and returns to the top level
server(Port):-
	http_server(http_dispatch, [port(Port)]).

% Handler that finds all the strategies in the system and replies with a json
strategies(Request):-
	member(method(get), Request), !,
	find_strategies(Strategies),
	reply_json_dict(strategies{success: true, status: 200, strategies: Strategies}).

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
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

% Handles a request to create a new generation in the logic base
generation(Request):-
	member(method(put), Request), !,
	http_read_json_dict(Request, DictIn),
	new_generation(DictIn, Success),
	http_log('Success: ~w~n', [Success]),
	( Success == true -> 
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

% Handles a request to add a new interaction percept to an agent
percept_interaction(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_interaction_percept(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

% Hanldes a request to add a new action interaction percept to an agent
percept_action_interaction(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_action_interaction_percept(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).


% Hanldes a request to add a new action interaction percept to an agent
percept_action_gossip(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_action_gossip_percept(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

% Handles a request to check the belief of an agent on the last time they were a donor
belief_donor(Request):-
	member(method(get), Request), !,
	http_parameters(
		Request,
		[
			timepoint(Timepoint, [integer]),
			community(Community, [integer]),
			generation(Generation, [integer]),
			player(AgentID, [integer])
		]
	),
	get_donor_belief(Community, Generation, AgentID, Timepoint, Success, Value, RecipientID),
	(Success == true ->
		reply_json(return{data:
			data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: true, status: 200, timepoint: Value, recipient: RecipientID}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: false, status: 200, message: Success})
	).

% Handles a request to check the belief of an agent on the last time they were a donor
belief_recipient(Request):-
	member(method(get), Request), !,
	http_parameters(
		Request,
		[
			timepoint(Timepoint, [integer]),
			community(Community, [integer]),
			generation(Generation, [integer]),
			player(AgentID, [integer])
		]
	),
	get_recipient_belief(Community, Generation, AgentID, Timepoint, Success, Value, DonorID),
	(Success == true ->
		reply_json(return{data:
			data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: true, status: 200, timepoint: Value, donor: DonorID}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: false, status: 200, message: Success})
	).

% Handles a request to check the belief of two agents on when they last interacted and what roles they took in the donor-recipient pair
belief_interaction(Request):-
	member(method(get), Request), !,
	http_parameters(
		Request,
		[
			timepoint(Timepoint, [integer]),
			community(Community, [integer]),
			generation(Generation, [integer]),
			player1(Agent1ID, [integer]),
			player2(Agent2ID, [integer])
		]
	),
	get_interaction_belief(Community, Generation, Timepoint, Agent1ID, Agent2ID, Success, Value, DonorID, RecipientID),
	(Success == true ->
		reply_json(return{data:
						data{community: Community, generation: Generation, player1: Agent1ID, player2: Agent2ID, timepoint: Timepoint},
					success: true, status: 200, timepoint: Value, donor: DonorID, recipient: RecipientID}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation,  player1: Agent1ID, player2: Agent2ID, timepoint: Timepoint},
					success: false, status: 200, message: Success})
	).

belief_standing(Request):-
	member(method(get), Request), !,
	http_parameters(
		Request,
		[
			timepoint(Timepoint, [integer]),
			community(Community, [integer]),
			generation(Generation, [integer]),
			perceiver(PerceiverID, [integer]),
			about(AboutID, [integer])
		]
	),
	get_standing_belief(Community, Generation, Timepoint, PerceiverID, AboutID, Success, Standing),
	(Success == true ->
		reply_json(return{data: 
						data{community: Community, generation: Generation, perceiver: PerceiverID, about: AboutID, timepoint: Timepoint},
					success: true, status: 200, standing: Standing}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation, perceiver: PerceiverID, about: AboutID, timepoint: Timepoint},
					success: false, status: 200, message: Success})
	).

action(Request):-
	member(method(get), Request), !,
	http_parameters(
		Request,
		[
			timepoint(Timepoint, [integer]),
			community(Community, [integer]),
			generation(Generation, [integer]),
			player(AgentID, [integer])
		]
	),
	agent_action(Timepoint, Community, Generation, AgentID, Success, Action),
	(Success == true ->
		reply_json(return{data:
			data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: true, status: 200, action: Action}) ;
		reply_json(return{data:
			data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: false, status: 200, message: Success})
	).