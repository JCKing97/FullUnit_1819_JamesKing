
 /** <module> This file handles the inputting of percepts to agents.
 * @author James King
 */

?- ["./strategies"].
?- ["./communities"].
:- use_module(library(http/http_log)).
% Compile and set up mvfcec
?- ["./mvfcec/src/lib/utilities"].
?- ["./mvfcec/src/compiler/basic_V1.0"].
?- ["./mvfcec/src/lib/activity_recognition_lifecycles"].
:- dynamic observed_at/2.

/*-------------------------------------
----- Action Interaction Percept ------
-------------------------------------*/

/**
 * add_new_action_interaction_percept(++DictIn:dict, --Success:atom) is nondet
 *
 * Add a new action interaction percept to the agent specified in the dictionary with the parameters specified in the dictionary,
 * an action interaction percept is an observation of an interaction where a donor either defect or cooperated with a recipient,
 * the api docs specify the layout of the dict.
 *
 * @arg DictIn The dictionary containing agent and percept information
 * @arg Success Whether the input of a percept was successful (true) or not (error message)
 */

% Add an action percept to a recipient
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	agent(PerceiverStrat, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	DonorID = DictIn.donor,
	RecipientID = DictIn.recipient,
	RecipientID \== DonorID,
	agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
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
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	Action = DictIn.action,
	\+( Action == "defect" ; Action == "cooperate" ),
	Success =  "Incorrect action must either be defect or cooperate", !.
% Fail nicely when no such community
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = "No such community", !.
% Fail nicely when so such generation for this community
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = "No such generation for this community", !.
% Fail nicely when no such perceiver, donor abd recipient
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
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
	Success = "No such agent for: perceiver, donor or recipient", !.
% Fail nicely when no such perceiver and donor
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	Success = "No such agent for: perceiver or donor", !.
% Fail nicely when no such perceiver, and recipient
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = "No such agent for: perceiver or recipient", !.
% Fail nicely when no such donor and recipient
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = "No such agent for: donor or recipient", !.
% Fail nicely when no such perceiver
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	Success = "No such agent for: perceiver", !.
% Fail nicely when no such donor
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	Success = "No such agent for: donor", !.
% Fail nicely when no such recipient
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = "No such agent for: recipient", !.
% Fail nicely if donor and recipient are the same
add_new_action_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	DonorID = DictIn.donor,
	RecipientID = DictIn.recipient,
	DonorID == RecipientID,
	Success = "Incorrect IDs: Donor and Recipient are the same", !.
% Fail nicely if input doesn"t contain the correct fields
add_new_action_interaction_percept(DictIn, Success):-
	\+ _{community: _, generation: _, perceiver: _, donor: _, recipient: _, timepoint: _, action: _} :< DictIn,
	Success = "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action", !.

/*------------------------------
------- Gossip Percept ---------
------------------------------*/

/**
 * add_new_action_gossip_percept(++DictIn:dict, --Success:atom) is nondet
 *
 * Add a new action gossip percept to the agent specified in the dictionary with the parameters specified in the dictionary,
 * an action gossip percept is an receiving of gossip by a perceiver from another agent (gossiper) about another agent,
 * the api docs specify the layout of the dict.
 *
 * @arg DictIn The dictionary containing agent and percept information
 * @arg Success Whether the input of a percept was successful (true) or not (error message)
 */

% Add a gossip percepts to a recipient
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
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
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	Gossip = DictIn.gossip,
	\+( Gossip == "positive" ; Gossip == "negative"),
	Success = "Incorrect gossip action should be either positive or negative", !.
% fail nicely if no such community
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = "No such community", !.
% Fail nicely if no such generation in this community
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = "No such generation for this community", !.
% Fail nicely when no such perceiver, about or gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
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
	Success = "No such agent for: perceiver, gossiper or about", !.
% Fail nicely when no such perceiver, gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = "No such agent for: perceiver or gossiper", !.
% Fail nicely when no such perceiver or about agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	Success = "No such agent for: perceiver or about", !.
% Fail nicely when no such about or gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = "No such agent for: gossiper or about", !.
% Fail nicely when no such perceiver agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	PerceiverId = DictIn.perceiver,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), PerceiverId),
	Success = "No such agent for: perceiver", !.
% Fail nicely when no such about agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	AboutID = DictIn.about,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	Success = "No such agent for: about", !.
% Fail nicely when no such gossiper agent
add_new_action_gossip_percept(DictIn, Success):-
	_{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	GossiperID = DictIn.gossiper,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GossiperID),
	Success = "No such agent for: gossiper", !.
% Fail nicely with incorrect input dictionary
add_new_action_gossip_percept(DictIn, Success):-
	\+ _{community: _, generation: _, perceiver: _, about: _, gossiper: _, timepoint: _, gossip: _} :< DictIn,
	Success = "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip", !.

/*------------------------------
----- Interaction Percept ------
------------------------------*/

/**
 * add_new_interaction_percept(++DictIn:dict, --Success:atom) is nondet
 *
 * Add a new percept that the specified donor and recipient agents are in an interaction together at the specified timepoint,
 * the api docs specify the layout of the dict.
 *
 * @arg DictIn The dictionary containing agent and percept information
 * @arg Success Whether the input of a percept was successful (true) or not (error message)
 */

% Add an interaction percept
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	RecipientID = DictIn.recipient,
	DonorID \== RecipientID,
	agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Timepoint = DictIn.timepoint,
	assert(observed_at(interaction(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint)),
	update_at(interaction(agent(DonorStrat, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
		agent(RecipientStrat, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint),
	Success = true, !.
% Fail nicely if no such community
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	CommunityID = DictIn.community,
	\+community(CommunityID),
	Success = "No such community", !.
% Fail nicely if no such generation for this community
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	\+generation(community(CommunityID), GenerationID),
	Success = "No such generation for this community", !.
% Fail nicely if no such agent that is the donor and recipient
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = "No such recipient or donor for this community and generation", !.
% Fail nicely if nor such agent that is the donor
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	DonorID = DictIn.donor,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), DonorID),
	Success = "No such donor for this community and generation", !.
% Fail nicely if no such agent that is the recipient
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	CommunityID = DictIn.community,
	community(CommunityID),
	GenerationID = DictIn.generation,
	generation(community(CommunityID), GenerationID),
	RecipientID = DictIn.recipient,
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	Success = "No such recipient for this community and generation", !.
% Fail nicely if donor and recipient match
add_new_interaction_percept(DictIn, Success):-
	_{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	DonorID = DictIn.donor,
	RecipientID = DictIn.recipient,
	RecipientID == DonorID,
	Success = "Donor should not be the same as the recipient", !.
% Fail nicely if the input is incorrect
add_new_interaction_percept(DictIn, Success):-
	\+ _{community: _, generation: _, donor: _, recipient: _, timepoint: _} :< DictIn,
	Success = "Incorrect input, must contain: community, generation, donor, recipient, timepoint", !.