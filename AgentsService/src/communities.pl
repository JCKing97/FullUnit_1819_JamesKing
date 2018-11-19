/*--------------------------------------
Author:		James King
Title:		communities.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to creating communities
--------------------------------------*/

:- module(communities, [new_community/1, new_generation/2, community/1, generation/2]).
:- dynamic community/1, generation/2.

new_community(ID):-
	get_new_id(ID),
	assert(communities:community(ID)).

get_new_id(NewID):-
	current_predicate(id/1),
	id(ID), !,
	retract(id(ID)),
	NewID is ID+1,
	assert(id(NewID)).
get_new_id(ID):-
	assert(id(0)),
	ID is 0.
	
new_generation(DictIn, Status):-
	current_predicate(generation/2),
	current_predicate(community/1),
	CommunityID = DictIn.community,
	Community = communities:community(CommunityID),
	GenerationID = DictIn.generation,
	(generation(CommunityID, GenerationID) -> fail ;
	assert(communities:generation(Community, GenerationID)),
	Status = "Good"), !.
new_generation(DictIn, Status):-
    \+current_predicate(generation/2),
	current_predicate(community/1),
	CommunityID = DictIn.community,
	Community = communities:community(CommunityID),
	GenerationID = DictIn.generation,
	assert(communities:generation(Community, GenerationID)),
	Status = "Good", !.
new_generation(_, Status):-
	Status = "Bad".
