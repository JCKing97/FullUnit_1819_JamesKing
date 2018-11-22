/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to querying agents beliefs
--------------------------------------*/

% Get the value of a belief at a given timepoint

% When did this agent last believe they were a donor?
get_belief(DictIn, Value):-
	Type = DictIn.type,
	Type == "donor",
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	AgentID = DictIn.player,
	holds_at(last_interaction_timepoint(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), _)=Value, Timepoint), !.
% When did this agent last believe they were a recipient
get_belief(DictIn, Value):-
	Type = DictIn.type,
	Type == "recipient",
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	AgentID = DictIn.player,
	holds_at(last_interaction_timepoint(_,agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID))=Value, Timepoint), !.
% When did these agents last believe they were part of an interaction together?
get_belief(DictIn, Value):-
	Type = DictIn.type,
	Type == "interaction",
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	Agent1ID = DictIn.player1,
	Agent2ID = DictIn.player2,
	holds_at(last_interaction_timepoint(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), Agent1ID),
	 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), Agent2ID))=Value, Timepoint), !.
get_belief(DictIn, Value):-
	Type = DictIn.type,
	Type == "interaction",
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	Agent1ID = DictIn.player1,
	Agent2ID = DictIn.player2,
	holds_at(last_interaction_timepoint(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), Agent2ID),
	 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), Agent1ID))=Value, Timepoint), !.
% What did the perceiver believe about the standing of this other agent?
get_belief(DictIn, Value):-
	Type = DictIn.type,
	Type == "standing",
	Timepoint = DictIn.timepoint,
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	PerceiverID = DictIn.perceiver,
	AboutID = DictIn.about,
	agent(strategy("Standing Discriminator", StratDesc, StratOptions), community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverID),
	agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	holds_at(standing(agent(strategy("Standing Discriminator", StratDesc, StratOptions), community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverID),
		agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID))=Value, Timepoint), !.
% If there is no holds_at value but the perceiver uses the standing strategy they will autobelieve them to be of a good standing
get_belief(DictIn, Value):-
	Type = DictIn.type,
	Type == "standing",
	CommunityID = DictIn.community,
	GenerationID = DictIn.generation,
	PerceiverID = DictIn.perceiver,
	AboutID = DictIn.about,
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverID),
	agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	Value=good, !.
% There is no applicable belief
get_belief(_, "No applicable value").