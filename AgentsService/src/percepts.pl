/*--------------------------------------
Author:		James King
Title:		percepts.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to adding percepts to an agents memory
--------------------------------------*/
?- ['./strategies'].
?- ['./communities'].
:- use_module(library(http/http_log)).

/*-------------------------------------
----- Action Interaction Percept ------
-------------------------------------*/

% Add an action percepts to a recipient
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	DonorID = DictIn.donor,
	agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Timepoint = DictIn.timepoint,
	Action = DictIn.action,
	( Action == "defect" ; Action == "cooperate" ),
	assert(observed_at(did(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID), 
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId), 
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
		Action), Timepoint)),
	update_at(did(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID), 
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId), 
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
		Action), Timepoint),
	Success = true, !.
% Fail nicely if incorrect action
add_new_action_interaction_percept(DictIn, Success):-
	Action = DictIn.action,
	\+( Action == "defect" ; Action == "cooperate" ),
	Success =  'Incorrect action must either be defect or cooperate', !.
% Fail nicely when no such community
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = 'No such community', !.
% Fail nicely when so such generation for this community
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = 'No such generation for this community', !.
% Fail nicely when no such perceiver, donor abd recipient
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = 'No such agent for: perceiver, donor or recipient', !.
% Fail nicely when no such perceiver and donor
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	Success = 'No such agent for: perceiver or donor', !.
% Fail nicely when no such perceiver, and recipient
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = 'No such agent for: perceiver or recipient', !.
% Fail nicely when no such donor and recipient
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = 'No such agent for: donor or recipient', !.
% Fail nicely when no such perceiver
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	Success = 'No such agent for: perceiver', !.
% Fail nicely when no such donor
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	Success = 'No such agent for: donor', !.
% Fail nicely when no such recipient
add_new_action_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = 'No such agent for: recipient', !.

/*------------------------------
------- Gossip Percept ---------
------------------------------*/

% Add a gossip percepts to a recipient
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	AboutID = DictIn.about,
	agent(AboutStrat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	GossiperID = DictIn.gossiper,
	agent(GossiperStrat, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Timepoint = DictIn.timepoint,
	Gossip = DictIn.gossip,
	( Gossip == "positive" ; Gossip == "negative"),
	assert(observed_at(said(agent(GossiperStrat, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
		agent(AboutStrat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
		Gossip), Timepoint)),
	update_at(said(agent(GossiperStrat, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
		agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
		agent(AboutStrat, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
		Gossip), Timepoint),
	Success = true, !.
% Fail nicely if incorrect gossip
add_new_action_gossip_percept(DictIn, Success):-
	Gossip = DictIn.gossip,
	\+( Gossip == "positive" ; Gossip == "negative"),
	Success = 'Incorrect gossip action should be either positive or negative', !.
% fail nicely if no such community
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = 'No such community', !.
% Fail nicely if no such generation in this community
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = 'No such generation for this community', !.
% Fail nicely when no such perceiver, about or gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = 'No such agent for: perceiver, gossiper or about', !.
% Fail nicely when no such perceiver, gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = 'No such agent for: perceiver or gossiper', !.
% Fail nicely when no such perceiver or about agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	Success = 'No such agent for: perceiver or about', !.
% Fail nicely when no such about or gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = 'No such agent for: gossiper or about', !.
% Fail nicely when no such perceiver agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	Success = 'No such agent for: perceiver', !.
% Fail nicely when no such about agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	Success = 'No such agent for: about', !.
% Fail nicely when no such gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = 'No such agent for: gossiper', !.

/*------------------------------
----- Interaction Percept ------
------------------------------*/

% Add an interaction percept
add_new_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Timepoint = DictIn.timepoint,
	assert(observed_at(interaction(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint)),
	update_at(interaction(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint),
	Success = true, !.
% Fail nicely if no such community
add_new_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = 'No such community', !.
% Fail nicely if no such generation for this community
add_new_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = 'No such generation for this community', !.
% Fail nicely if no such agent that is the donor and recipient
add_new_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = 'No such recipient or donor for this community and generation', !.
% Fail nicely if nor such agent that is the donor
add_new_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	Success = 'No such donor for this community and generation', !.
% Fail nicely if no such agent that is the recipient
add_new_interaction_percept(DictIn, Success):-
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = 'No such recipient for this community and generation', !.