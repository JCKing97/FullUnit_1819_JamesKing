/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to agents strategies
--------------------------------------*/

:- module(strategies, [agent_action//2, new_agent//3, find_strategies//1]).

strategy( "cooperator" , 'cooperates every time').
strategy( "defector" , 'defects every time').
strategy_action( "defector", _, false).
strategy_action( "cooperator", _, true).

agent_action(Dict, Action):-
	DonorID = Dict.donorID,
	RecipientID = Dict.recipientID,
	agent(DonorStrategy, DonorID),
	strategy_action(DonorStrategy, RecipientID, Action).

find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc}, strategy(Name, Desc), Strategies).

get_new_id(NewID):-
	current_predicate(id/1),
	id(ID), !,
	retract(id(ID)),
	NewID is ID+1,
	assert(id(NewID)).
get_new_id(ID):-
	assert(id(0)),
	ID is 0.

new_agent(DictIn, ID, Status):-
	Strategy = DictIn.strategy,
	strategy(Strategy,_),
	get_new_id(ID),
	assert(agent(Strategy, ID)),
	Status = "Good".
new_agent(_, _, Status):-
	Status = "Bad".
