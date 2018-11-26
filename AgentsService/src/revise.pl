/*--------------------------------------
Author:		James King
Title:		revise.pl
Created:	20th Nov 2018
Desc:		Contains the logic related to revising agents beliefs using the events calculus
--------------------------------------*/

:- use_module(library(lists)).

% Some logic
get_strategy(Strategy, Options, agent(strategy(Strategy, _, Options), _, _, _)).

/*-----------------------------
----------- General -----------
-----------------------------*/

% Change when the last interaction occurred between two agents
initiates_at(interaction(agent(strategy(_, _, _), Community, Generation, Donor), agent(strategy(_, _, _), Community, Generation, Recipient)), [],
  last_interaction_timepoint(agent(strategy(_, _, _), Community, Generation, Donor), agent(strategy(_, _, _), Community, Generation, Recipient))=T, T):-
	happens_at(interaction(agent(strategy(_, _, _), Community, Generation, Donor), agent(strategy(_, _, _), Community, Generation, Recipient)), T).

% Is nothing ok?
causes_at(said(_, _,_,_), nothing, _).
causes_at(did(_, _,_,_), nothing, _).
causes_at(interaction(_, _), nothing, _).

/*-----------------------------
------ Standing Strategy ------
-----------------------------*/

% How actions affect the standing of a player
% If a donor defects against a recipient with good standing, give donor a bad standing to the perceiver
initiates_at(did(Donor, Perceiver, Recipient, Action), [], standing(Perceiver, Donor)=bad, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(Strategy, _, Perceiver),
	Strategy=="Standing Discriminator",
	Action == "defect",
	\+holds_at(standing(Perceiver, Recipient)=bad, T).

% If a donor cooperates with anyone, give donor a good standing to the perceiver
initiates_at(did(Donor, Perceiver, Recipient, Action), [], standing(Perceiver, Donor)=good, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(Strategy, _, Perceiver),
	Strategy=="Standing Discriminator",
	Action=="cooperate".

% How gossip affects the standing of a player
% If Gossiper is trusted (Perceiver is a trusting agent and holds Gossiper in good standing)
% And Gossip is negative Perceiver gives About bad standing
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(Strategy, Options, Perceiver),
	Strategy=="Standing Discriminator",
	member("trusting", Options),
	Gossip == "negative",
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).

% If Gossiper is trusted and Gossip is positive
% Perceiver gives About good standing
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=good, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(Strategy, Options, Perceiver),
	Strategy=="Standing Discriminator",
	member("trusting", Options),
	Gossip == "positive",
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).


/*-----------------------------
-------- Image Scoring --------
-----------------------------*/