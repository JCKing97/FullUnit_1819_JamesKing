/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to querying agents beliefs
--------------------------------------*/

% get the value of a belief at a given timepoint
get_belief(DictIn, Value):-
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	AgentID = DictIn.player,
	Type = DictIn.type,
	Type == "donor",
	holds_at(last_donor_timepoint(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID))=Value, Timepoint), !.
get_belief(_, "No applicable value").