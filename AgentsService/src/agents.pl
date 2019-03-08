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
	_{donor_strategy: StrategyName, non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, options: Options, community: CommunityID, generation: GenerationID, player: AgentID} :< DictIn,
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	strategy(StrategyName, NonDonorStrategy, TrustModel, Desc, Options),
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	( agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID) ->
	 Success = "Player ID already taken for this community and generation" ;
	assert(agent(strategy(StrategyName, NonDonorStrategy, TrustModel, Desc, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID)),
	Success = true), !.
% Create the first agent in the system
new_agent(DictIn, trust):-
	% Check input fields
	_{donor_strategy: StrategyName, non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, options: Options, community: CommunityID, generation: GenerationID, player: AgentID} :< DictIn,
	\+current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	strategy(StrategyName, NonDonorStrategy, TrustModel, Desc, Options),
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	assert(agent(strategy(StrategyName, NonDonorStrategy, TrustModel, Desc, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID)), !.
% Nice fail when there is no such strategy
new_agent(DictIn, "No such strategy"):-
	% Check input fields
	_{donor_strategy: StrategyName, non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, options: Options, community: _, generation: _, player: _} :< DictIn,
	\+strategy(StrategyName, NonDonorStrategy, TrustModel, _, Options), !.
% Nice fail when there is no such community
new_agent(DictIn, "No such community"):-
	% Check input fields
	_{donor_strategy: _, non_donor_strategy: _, trust_model: _, options: _, community: CommunityID, generation: _, player: _} :< DictIn,
	current_predicate(community/1),
	\+community(CommunityID), !.
new_agent(DictIn, "No such community"):-
	% Check input fields
	_{donor_strategy: _, non_donor_strategy: _, trust_model: _, options: _, community: _, generation: _, player: _} :< DictIn,
	\+current_predicate(community/1),!.
% Nice fail when here is no such generation for this community
new_agent(DictIn, "No such generation for this community"):-
	% Check input fields
	_{donor_strategy: _, non_donor_strategy: _, trust_model: _, options: _, community: CommunityID, generation: GenerationID, player: _} :< DictIn,
	current_predicate(community/1),
	current_predicate(generation/2),
	community(CommunityID),
	\+generation(community(CommunityID), GenerationID), !.
new_agent(DictIn, "No such generation for this community"):-
	% Check input fields
	_{donor_strategy: _, non_donor_strategy: _, trust_model: _, options: _, community: _, generation: _, player: _} :< DictIn,
	current_predicate(community/1),
	\+current_predicate(generation/2), !.
% Nice failing for incorrect inputs
new_agent(DictIn, "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player"):-
	\+ _{donor_strategy: _, non_donor_strategy: _, trust_model: _, options: _, community: _, generation: _, player: _} :< DictIn, !.

/**
 * retract_agents(++ID:int) is nondet
 *
 * Delete all the agents in the service related to the community ID passed in.
 *
 * @arg ID The ID of the community all the agents you wish to retract belongs to
 */
 retract_agents(ID):-
	retractall(agent(_, community(ID), _, _)).