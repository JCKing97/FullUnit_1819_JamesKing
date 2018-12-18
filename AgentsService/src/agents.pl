/** <module> This file handles the managing of agents in the service
 * @author James King
 */

:- dynamic agent/4, community/1, generation/2.
:- multifile agent/4, community/1, generation/2.
:- use_module(library(http/http_log)).
?- ['./strategies'].

/**
 * new_agent(++DictIn:dict, -Success:atom) is nondet
 *
 * Create a new agent in the service, fails if DictIn is not in the format specified in the API documentation,
 * Success may be an atom if the strategy, community, generation or agent is incorrect in DictIn, Success becomes an error message in this case.
 *
 * @arg DictIn The dictionary containing details on the new agents strategy (+options), community id, generation id and agent id as specified in the api documentation
 * @arg Success An output argument whether creating the agent was successful (becomes true) or not (becomes error message)
 */

% Create a new agent if there are no other agents with the same community, generation and agent id
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	StrategyName = DictIn.strategy,
	Options = DictIn.options,
	strategy(StrategyName, Desc, Options),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	( agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID) ->
	 Success = "Player ID already taken for this community and generation" ;
	assert(agent(strategy(StrategyName, Desc, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID)),
	Success = true), !.
% Create the first agent in the system
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	\+current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	StrategyName = DictIn.strategy,
	Options = DictIn.options,
	strategy(StrategyName, Desc, Options),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	assert(agent(strategy(StrategyName, Desc, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID)),
	Success = true, !.
% Nice fail when there is no such strategy
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	StrategyName = DictIn.strategy,
	Options = DictIn.options,
	\+strategy(StrategyName, _, Options),
	Success = "No such strategy", !.
% Nice fail when there is no such community
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	current_predicate(community/1),
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = "No such community", !.
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	\+current_predicate(community/1),
	Success = "No such community", !.
% Nice fail when here is no such generation for this community
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	current_predicate(community/1),
	current_predicate(generation/2),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = "No such generation for this community", !.
new_agent(DictIn, Success):-
	% Check input fields
	_{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	current_predicate(community/1),
	\+current_predicate(generation/2),
	Success = "No such generation for this community", !.
% Nice failing for incorrect inputs
new_agent(DictIn, Success):-
	\+ _{strategy: _, options: _, community: _, generation: _, player: _} :< DictIn,
	Success = "Incorrect input, should contain the fields: strategy, options, community, generation and player".

/**
 * retract_agents(++ID:int) is nondet
 *
 * Delete all the agents in the service related to the community ID passed in.
 *
 * @arg ID The ID of the community all the agents you wish to retract belongs to
 */
 retract_agents(ID):-
	retractall(agent(_, community(ID), _, _)).