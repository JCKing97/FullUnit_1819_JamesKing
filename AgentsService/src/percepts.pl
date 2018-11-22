/*--------------------------------------
Author:		James King
Title:		percepts.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to adding percepts to an agents memory
--------------------------------------*/
?- ['./strategies'].
?- ['./communities'].
:- use_module(library(http/http_log)).

% Add an action percepts to a recipient
add_new_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "action",
	Action_type = Percept.content.type,
	Action_type == "interaction",
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiverId,
	agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	Action = Percept.content.action,
	DonorID = Percept.content.donor,
	agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = Percept.content.recipient,
	agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Timepoint = DictIn.timepoint,
	assert(observed_at(did(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID), 
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId), 
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
		Action), Timepoint)),
	update_at(did(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID), 
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId), 
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
		Action), Timepoint),
	Status = "Good", !.
% Add a gossip percepts to a recipient
add_new_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "action",
	Action_type = Percept.content.type,
	Action_type == "gossip",
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiverId,
	agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	Gossip = Percept.content.gossip,
	AboutID = Percept.content.about,
	agent(AboutStrat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	GossiperID = Percept.content.gossiper,
	agent(GossiperStrat, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Timepoint = DictIn.timepoint,
	assert(observed_at(said(agent(GossiperStrat, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
		agent(AboutStrat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
		Gossip), Timepoint)),
	update_at(said(agent(GossiperStrat, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
		agent(AboutStrat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
		Gossip), Timepoint),
	Status = "Good", !.
% Add an interaction percept
add_new_percept(DictIn, Status):-
	Percept = DictIn.percept,
	Type = Percept.type,
	Type == "interaction",
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = Percept.donor,
	agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = Percept.recipient,
	agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Timepoint = DictIn.timepoint,
	assert(observed_at(interaction(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint)),
	update_at(interaction(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint),
	Status = "Good", !.
% Nice fail as there is no such percept
add_new_percept(_, Status):-
	Status = "Bad".
