/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to agents strategies
--------------------------------------*/

:- module(strategies, [agent_action//3, new_agent//3, find_strategies//1]).

:- use_module(communities).

strategy( "cooperator" , 'cooperates every time').
strategy( "defector" , 'defects every time').
strategy_action( "defector", _, false).
strategy_action( "cooperator", _, true).

agent_action(DictIn, Action, Status):-
	DonorID = DictIn.donor,
	RecipientID = DictIn.recipient,
	CommunityID = DictIn.community,
	communities:community(CommunityID),
	GenerationID = DictIn.generation,
	communities:generation(CommunityID, GenerationID),
	agent(DonorStrategy, CommunityID, GenerationID, DonorID),
	agent(_, CommunityID, GenerationID, RecipientID),
	strategy_action(DonorStrategy, RecipientID, Action),
	Status = "Good".
agent_action(_, _, Status):-
    	Status = "Bad".

find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc}, strategy(Name, Desc), Strategies).

new_agent(DictIn, Status):-
	current_predicate(agent/4),
	current_predicate(communities:community/1),
	current_predicate(communities:generation/2),
	Strategy = DictIn.strategy,
	strategy(Strategy, _),
	CommunityID = DictIn.community,
	communities:community(CommunityID),
	GenerationID = DictIn.generation,
	communities:generation(CommunityID, GenerationID),
	AgentID = DictIn.player,
	( agent(_, CommunityID, GenerationID, AgentID) -> fail ;
	assert(agent(Strategy, CommunityID, GenerationID, AgentID)),
	Status = "Good").
new_agent(DictIn, Status):-
	\+current_predicate(agent/4),
	current_predicate(communities:community/1),
	current_predicate(communities:generation/2),
	Strategy = DictIn.strategy,
	strategy(Strategy, _),
	CommunityID = DictIn.community,
	communities:community(CommunityID),
	GenerationID = DictIn.generation,
	communities:generation(CommunityID, GenerationID),
	AgentID = DictIn.player,
	assert(agent(Strategy, CommunityID, GenerationID, AgentID)),
	Status = "Good".
new_agent(_, Status):-
	Status = "Bad".
