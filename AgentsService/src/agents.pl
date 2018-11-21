/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	Nov 2018
Desc:		Contains the logic related to agents
--------------------------------------*/

?- ['./communities'].
:- dynamic agents/4.

new_agent(DictIn, Status):-
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
	( agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID) -> fail ;
	assert(agent(strategy(StrategyName, Desc, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID)),
	Status = "Good"), !.
new_agent(DictIn, Status):-
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
	Status = "Good", !.
new_agent(_, Status):-
	Status = "Bad".
