/*--------------------------------------
Author:		James King
Title:		percepts.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to adding percepts to an agents memory
--------------------------------------*/
:- use_module(strategies).
:- use_module(communities).
:- use_module(library(http/http_log)).
:- multifile happens_at/2, initiates_at/4, terminates_at/4.

add_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "action",
	Action_type = Percept.content.type,
	Action_type == "interaction",
	CommunityID = DictIn.community,
	Community = communities:community(CommunityID),
	GenerationID = DictIn.generation,
	Generation = communities:generation(Community, GenerationID),
	PerceiverId = DictIn.perceiverId,
	Perceiver = strategies:agent(_, Community, Generation, PerceiverId),
	Timepoint = DictIn.timepoint,
	CooperatedOrNot = Percept.content.cooperated,
	DonorID = Percept.content.donor,
	Donor = strategies:agent(_, Community, Generation, DonorID),
	RecipientID = Percept.content.recipient,
	Recipient = strategies:agent(_, Community, Generation, RecipientID),
	l_cautious_assert(happens_at(observed(Perceiver, did(Donor, Recipient, CooperatedOrNot)), Timepoint)),
	Status = "Good", !.
add_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "action",
	Action_type = Percept.content.type,
	Action_type == "gossip",
	CommunityID = DictIn.community,
	Community = communities:community(CommunityID),
	GenerationID = DictIn.generation,
	Generation = communities:generation(Community, GenerationID),
	PerceiverId = DictIn.perceiverId,
	Perceiver = strategies:agent(_, Community, Generation, PerceiverId),
	PositiveOrNot = Percept.content.positive,
	AboutID = Percept.content.about,
	About = strategies:agent(_, Community, Generation, AboutID),
	GossiperID = Percept.content.gossiper,
	Gossiper = strategies:agent(_, Community, Generation, GossiperID),
	Timepoint = DictIn.timepoint,
	l_cautious_assert(happens_at(observed(Perceiver, said(Gossiper, About, PositiveOrNot)), Timepoint)),
	Status = "Good", !.
add_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "donor",
	CommunityID = DictIn.community,
	Community = communities:community(CommunityID),
	GenerationID = DictIn.generation,
	Generation = communities:generation(Community, GenerationID),
	PerceiverId = DictIn.perceiverId,
	Perceiver = strategies:agent(_, Community, Generation, PerceiverId),
	Timepoint = DictIn.timepoint,
	l_cautious_assert(happens_at(observed(Perceiver, donor(Perceiver)), Timepoint)),
	Status = "Good", !.
add_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "recipient",
	CommunityID = DictIn.community,
	Community = communities:community(CommunityID),
	GenerationID = DictIn.generation,
	Generation = communities:generation(Community, GenerationID),
	PerceiverId = DictIn.perceiverId,
	Perceiver = strategies:agent(_, Community, Generation, PerceiverId),
	Timepoint = DictIn.timepoint,
	l_cautious_assert(happens_at(observed(Perceiver, recipient(Perceiver)), Timepoint)),
	Status = "Good", !.
add_percept(_, Status):-
	Status = "Bad".

initiates_at(observed(Perceiver, said(Gossiper, About, Positive)), [], bad_standing(Perceiver, About), T):-
	happens_at(observed(Perceiver, said(Gossiper, About, Positive)), T),
	Positive == false,
	\+holds_at(bad_standing(Perceiver, Gossiper), T).
terminates_at(observed(Perceiver, said(Gossiper, About, Positive)), [], bad_standing(Perceiver, About), T):-
	happens_at(observed(Perceiver, said(Gossiper, About, Positive)), T),
	Positive == true,
	\+holds_at(bad_standing(Perceiver, Gossiper), T).
