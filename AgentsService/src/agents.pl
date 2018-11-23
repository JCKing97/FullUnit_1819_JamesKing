/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	Nov 2018
Desc:		Contains the logic related to agents
--------------------------------------*/

?- ['./communities'].
:- dynamic agents/4.
:- use_module(library(http/http_log)).

% Create a new agent if there are no other agents with the same community, generation and agent id
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
% Create the first agent in the system
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
% Fail to create a new player, maybe a bad input?
new_agent(_, Status):-
	Status = "Bad".

/*----------------------------------------------------------------------------------------
---------------------------------------- Get Actions -------------------------------------
----------------------------------------------------------------------------------------*/

/*-----------------------------
----------- Defector ----------
-----------------------------*/

% If the agent is a donor this turn
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Defector", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	http_log("Timepoint ~d~n", [Timepoint]),
	CheckTimepoint is Timepoint-1,
	holds_at(last_interaction_timepoint(agent(strategy("Defector", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),_)=CheckTimepoint, Timepoint),
	http_log("Timepoint-1 ~d~n", [Timepoint-1]),
	Action = "defect", !.	
% Auto to idle if not a donor
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Defector", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Action = "idle", !.	

/*-----------------------------
---------- Cooperator ---------
-----------------------------*/

% If the agent is a donor this turn
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Cooperator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	http_log("Timepoint ~d~n", [Timepoint]),
	CheckTimepoint is Timepoint-1,
	holds_at(last_interaction_timepoint(agent(strategy("Cooperator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),_)=CheckTimepoint, Timepoint),
	http_log("Timepoint-1 ~d~n", [Timepoint-1]),
	Action = "cooperate", !.
% Auto to idle if not a donor
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Cooperator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Action = "idle", !.

/*-----------------------------
------ Standing Strategy ------
-----------------------------*/

% If the agent is a donor this turn and holds the recipient in good standing: cooperate
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	http_log("Timepoint ~d~n", [Timepoint]),
	CheckTimepoint is Timepoint-1,
	holds_at(last_interaction_timepoint(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		Recipient)=CheckTimepoint, Timepoint),
	http_log("Timepoint-1 ~d~n", [Timepoint-1]),
	\+holds_at(standing(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), Recipient)=bad, Timepoint),
	Action = "cooperate", !.
% If the agent is a donor this turn and holds the recipient in bad standing: defect
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	http_log("Timepoint ~d~n", [Timepoint]),
	CheckTimepoint is Timepoint-1,
	holds_at(last_interaction_timepoint(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		Recipient)=CheckTimepoint, Timepoint),
	http_log("Timepoint-1 ~d~n", [Timepoint-1]),
	holds_at(standing(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), Recipient)=bad, Timepoint),
	Action = "defect", !.
% If the agent is a donor this turn and holds the recipient in bad standing: defect
agent_action(DictIn, Action):-
	current_predicate(agent/4),
	current_predicate(community/1),
	current_predicate(generation/2),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AgentID = DictIn.player,
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Action = "idle", !.

/*-----------------------------
----------- Failure -----------
-----------------------------*/

agent_action(_, "Fail").