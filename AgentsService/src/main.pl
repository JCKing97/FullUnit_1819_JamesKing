/** <module> This file links the system logic together compiling it all and setting up the server, this file also creates the uri endpoints and associates them with handlers, dealing with all the http work.
 * @author James King adapted from Anne Ogborn
*/

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
:- use_module(library(doc_files)).

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
:- http_handler(root(.), strategy, []).
:- http_handler(root(strategy), strategy, []).
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


/**
 * server(++Post:int) is semidet
 * 
 * Run this predicate with the port you wish to use it on as the Port.
 * @arg Port The port you wish the server to run on
 */
server(Port):-
	http_server(http_dispatch, [port(Port)]).

/**
 * strategies(++Request:list) is semidet
 *
 * The handler for the strategy route to reply with a list of strategies and some metadata.
 * @arg Request The request object passed from the HTTP request
 */
strategy(Request):-
	member(method(get), Request), !,
	find_strategies(Strategies),
	reply_json_dict(strategies{success: true, status: 200, strategies: Strategies}).

/**
 * community(++Request:list) is semidet
 *
 * The handler to create a new community in the service.
 * @arg Request The request object passed from the HTTP request
 */
community(Request):-
	member(method(post), Request), !,
	new_community(ID),
	reply_json(return{success: true, status: 200, id: ID}).

/**
 * generation(++Request:list) is nondet
 *
 * The handler to create a new generation in the service,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community doesn't exist or the generation for this community already exists.
 * @arg Request The request object passed from the HTTP request
 */
generation(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	new_generation(DictIn, Success),
	http_log('Success: ~w~n', [Success]),
	( Success == true -> 
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 	
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

/**
 * agent(++Request:list) is nondet
 *
 * The handler to create a new agent in the service,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community or generation for this community exists, or the agent id is already taken.
 * @arg Request The request object passed from the HTTP request
 */
agent(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	new_agent(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

/**
 * percept_interaction(++Request:list) is nondet
 *
 * The handler to give a new interaction percept (that they are in a donor-recipient pair) to the donor-recipient pair,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or agents do not exist.
 * @arg Request The request object passed from the HTTP request
 */
percept_interaction(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_interaction_percept(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

/**
 * percept_action_interaction(++Request:list) is nondet
 *
 * The handler to give a new action percept (the viewing of another interaction) to an onlooker,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or agents do not exist.
 * @arg Request The request object passed from the HTTP request
 */
percept_action_interaction(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_action_interaction_percept(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).


/**
 * percept_action_gossip(++Request:list) is nondet
 *
 * The handler to give a new gossip percept (the perceiving of gossip) to an recipient,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or agents do not exist.
 * @arg Request The request object passed from the HTTP request
 */
percept_action_gossip(Request):-
	member(method(post), Request), !,
	http_read_json_dict(Request, DictIn),
	add_new_action_gossip_percept(DictIn, Success),
	(Success == true ->
		reply_json(return{data: DictIn, success: Success, status: 200}) ; 
		reply_json(return{data: DictIn, success: false, message: Success, status: 200})).

/**
 * belief_donor(++Request:list) is nondet
 *
 * The handler to query an agents beliefs on when they have been a donor,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or agents do not exist.
 * @arg Request The request object passed from the HTTP request
 */
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
	get_donor_belief(Community, Generation, AgentID, Timepoint, Success, Value),
	(Success == true ->
		reply_json(return{data:
			data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: true, status: 200, interactions: Value}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: false, status: 200, message: Success})
	).

/**
 * belief_recipient(++Request:list) is nondet
 *
 * The handler to query an agents beliefs on when they have been a recipient,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or agents do not exist.
 * @arg Request The request object passed from the HTTP request
 */
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
	get_recipient_belief(Community, Generation, AgentID, Timepoint, Success, Value),
	(Success == true ->
		reply_json(return{data:
			data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: true, status: 200, interactions: Value}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation, player: AgentID, timepoint: Timepoint},
			success: false, status: 200, message: Success})
	).

/**
 * belief_interaction(++Request:list) is nondet
 *
 * The handler to query two agents beliefs on when they have been a part of a donor-recipient pair together,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or agents do not exist.
 * @arg Request The request object passed from the HTTP request
 */
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
	get_interaction_belief(Community, Generation, Timepoint, Agent1ID, Agent2ID, Success, Value),
	(Success == true ->
		reply_json(return{data:
						data{community: Community, generation: Generation, player1: Agent1ID, player2: Agent2ID, timepoint: Timepoint},
					success: true, status: 200, interactions: Value}) ;
		reply_json(return{data: 
						data{community: Community, generation: Generation,  player1: Agent1ID, player2: Agent2ID, timepoint: Timepoint},
					success: false, status: 200, message: Success})
	).

/**
 * belief_standing(++Request:list) is nondet
 *
 * The handler to query an agents beliefs on the standing of another agent,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation, agents do not exist or believer doesn't use the standing strategy.
 * @arg Request The request object passed from the HTTP request
 */
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

/**
 * action(++Request:list) is nondet
 *
 * The handler to get a commitment to an action from an agent at a particular timepoint,
 * fails if not passed the correct parameters as stipulated in the api docs,
 * responds unsuccessful to the client if the passed community, generation or the agent doesn't exist
 * @arg Request The request object passed from the HTTP request
 */
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