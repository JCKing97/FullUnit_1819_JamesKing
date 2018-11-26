/*--------------------------------------
Author:		James King
Title:		communities.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to creating communities
--------------------------------------*/

:- dynamic community/1, generation/2.

% Create a new community and return the ID
new_community(ID):-
	get_new_id(ID),
	assert(community(ID)).

% Get a new community id
get_new_id(NewID):-
	current_predicate(id/1),
	id(ID), !,
	retract(id(ID)),
	NewID is ID+1,
	assert(id(NewID)).
get_new_id(ID):-
	assert(id(0)),
	ID is 0.

% Create a new generation, if one with the same community and generation ID doesn't already exist
new_generation(DictIn, Success):-
	current_predicate(generation/2),
	current_predicate(community/1),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	(generation(community(CommunityID), GenerationID) -> Success = 'This community already has a generation with this id', ! ;
	assert(generation(community(CommunityID), GenerationID)),
	Success = true), !.
% Create the first generation in the system
new_generation(DictIn, Success):-
    \+current_predicate(generation/2),
	current_predicate(community/1),
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	assert(generation(community(CommunityID), GenerationID)),
	Success = true, !.
% Fail nicely when there is no such community
new_generation(DictIn, Success):-
	current_predicate(community/1),
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = 'No such community', !.
