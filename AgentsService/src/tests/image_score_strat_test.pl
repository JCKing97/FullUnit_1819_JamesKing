/** <module> This file tests any strategies related to image scoring
 * @author James King
 */

?- ['../percepts'].
?- ['../communities'].
?- ['../agents'].
?- ['../revise'].
?- ['../strategies'].
?- ['../beliefs'].
?- ['../actions'].

?-['../mvfcec/src/lib/utilities'].
?-['../mvfcec/src/compiler/basic_V1.0'].
?-['../mvfcec/src/lib/activity_recognition_lifecycles'].
:- dynamic observed_at/2.
dialect(swi).
:- dynamic player_id/1.

get_new_player_id(NewID):-
	current_predicate(player_id/1),
	player_id(ID), !,
	retract(player_id(ID)),
	NewID is ID+1,
	assert(player_id(NewID)).
get_new_player_id(ID):-
	assert(player_id(0)),
	ID is 0.

check_correct_action_timepoints([Timepoint], CommunityID, GenerationID, AgentID, Action):-
	assertion(agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action)),
	assertion(get_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action)).
check_correct_action_timepoints([Timepoint|OtherTimepoints], CommunityID, GenerationID, AgentID, Action):-
	assertion(agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action)),
	assertion(get_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action)),
	check_correct_action_timepoints(OtherTimepoints, CommunityID, GenerationID, AgentID, Action).

:- begin_tests(discriminator).

	test(image_score_beliefs_all_for_actions):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall(strategy("Image Scoring Discriminator", _, Options),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: Options, community: ID, generation: 0, player: PlayerID}, true),
				% Decrease every time a defection is seen
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -1, 2)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 2, action: "defect"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -2, 3)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 3, action: "defect"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -3, 4)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "defect"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -4, 5)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "defect"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -5, 6)),
				% Bound to -5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 6, action: "defect"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -5, 7)),
				% Increase image score by 1 every time cooperation observed
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 7, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -4, 8)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 7, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=1, 8)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 8, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=2, 9)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 9, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=3, 10)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 10, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=4, 11)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 11, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=5, 12)),
				% Bound to 5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 12, action: "cooperate"}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=5, 13)),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -4, 13))
		    )
		).

	test(image_score_beliefs_for_gossip_trusting):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall((strategy("Image Scoring Discriminator", _, Options), member("trusting", Options)),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: Options, community: ID, generation: 0, player: PlayerID}, true),
				% Decrease every time negative gossip is given about a player
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 1}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -1, 2)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 2}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -2, 3)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -3, 4)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 4}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -4, 5)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 5}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -5, 6)),
				% Bound by -5
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 6}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -5, 7)),
				% Increase image score by 1 everytime positive gossip is seen
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 7}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -4, 8)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 8}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=1, 9)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 9}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=2, 10)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 10}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=3, 11)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 11}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=4, 12)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 12}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=5, 13)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 13}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=5, 14)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 14}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -3, 15)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "negative", timepoint: 15}, true),
				assertion(holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=4, 16))
			)
		).

	test(image_score_beliefs_for_gossip_distrusting):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall((strategy("Image Scoring Discriminator", _, Options), member("distrusting", Options)),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: Options, community: ID, generation: 0, player: PlayerID}, true),
				% Do not allow the changing of any image score based on gossip
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 1}, true),
				assertion(\+holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -1, 2)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 2}, true),
				assertion(\+holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -2, 3)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 3}, true),
				assertion(\+holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, DefectID))= -1, 4)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 4}, true),
				assertion(\+holds_at(image_score(agent(_, community(ID), _, PlayerID), agent(_, community(ID), _, CoopID))=1, 5))
			)
		).

	test(image_score_interaction_actions_K0):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall(strategy("Image Scoring Discriminator", _, [0|OtherOptions]),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [0|OtherOptions], community: ID, generation: 0, player: PlayerID}, true),
				% Cooperates as presumes image score is 0
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 0}, true)),
				assertion(agent_action(0, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
		     	assertion(get_action(0, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 1}, true)),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 1}, true)),
				% Decrease image score by 1 each time, as these will be less than 0 defect
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 2}, true)),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 2, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 3}, true)),
				assertion(agent_action(3, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 3, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 4}, true)),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 5}, true)),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 6}, true)),
				assertion(agent_action(6, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(6, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				% Bound to -5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 6, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 7}, true)),
				assertion(agent_action(7, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(7, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),				
		     	% Increase image score by 1 every time cooperation observed, as greater than 0 cooperate
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 7, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 8}, true)),
				assertion(agent_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 9}, true)),
		     	assertion(agent_action(9, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(9, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 9, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 10}, true)),
				assertion(agent_action(10, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(10, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 10, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 11}, true)),
				assertion(agent_action(11, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(11, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 11, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 12}, true)),
				assertion(agent_action(12, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(12, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 12, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 13}, true)),
				assertion(agent_action(13, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(13, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 13, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 14}, true)),
				assertion(agent_action(14, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(14, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				% Bound to 5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 14, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 15}, true)),
				assertion(agent_action(15, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(15, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 16}, true)),
		     	assertion(agent_action(16, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(16, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID}))
		    )
		).

	test(image_score_interaction_actions_K2):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall(strategy("Image Scoring Discriminator", _, [2|OtherOptions]),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [2|OtherOptions], community: ID, generation: 0, player: PlayerID}, true),
				% Defect as presumes image score is 0 (less than 2)
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 0}, true)),
				assertion(agent_action(0, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(0, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 1}, true)),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID})),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 1}, true)),
				% Decrease every time a defection is seen, less than 2 therefore defect
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 2}, true)),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 2, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 3}, true)),
				assertion(agent_action(3, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 3, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 4}, true)),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 5}, true)),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 6}, true)),
				assertion(agent_action(6, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(6, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				% Bound to -5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 6, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 7}, true)),
				assertion(agent_action(7, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(7, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),				
		     	% Increase image score by 1 every time cooperation observed, defect until greather than 2
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 7, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 8}, true)),
				assertion(agent_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 9}, true)),
		     	assertion(agent_action(9, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID})),
		     	assertion(get_action(9, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 9, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 10}, true)),
				assertion(agent_action(10, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID})),
		     	assertion(get_action(10, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 10, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 11}, true)),
				assertion(agent_action(11, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(11, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 11, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 12}, true)),
				assertion(agent_action(12, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(12, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 12, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 13}, true)),
				assertion(agent_action(13, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(13, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 13, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 14}, true)),
				assertion(agent_action(14, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(14, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				% Bound to 5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 14, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 15}, true)),
				assertion(agent_action(15, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(15, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 16}, true)),
		     	assertion(agent_action(16, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(16, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID}))
		    )
		).

	test(image_score_interaction_actions_K_2):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall(strategy("Image Scoring Discriminator", _, [-2|OtherOptions]),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [-2|OtherOptions], community: ID, generation: 0, player: PlayerID}, true),
				% Cooperate as presumes image score is 0 (greather than -2)
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 0}, true)),
				assertion(agent_action(0, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
		     	assertion(get_action(0, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 1}, true)),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 1}, true)),
				% Decrease every time a defection is seen, cooperate until less than -2
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 2}, true)),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 2, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 3}, true)),
				assertion(agent_action(3, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 3, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 4}, true)),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 5}, true)),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 6}, true)),
				assertion(agent_action(6, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(6, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
				% Bound to -5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 6, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 7}, true)),
				assertion(agent_action(7, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(7, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),				
		     	% Increase image score by 1 every time cooperation observed, cooperate as greater than -2
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 7, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 8}, true)),
				assertion(agent_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 9}, true)),
		     	assertion(agent_action(9, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(9, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 9, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 10}, true)),
				assertion(agent_action(10, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(10, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 10, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 11}, true)),
				assertion(agent_action(11, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(11, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 11, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 12}, true)),
				assertion(agent_action(12, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(12, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 12, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 13}, true)),
				assertion(agent_action(13, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(13, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 13, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 14}, true)),
				assertion(agent_action(14, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(14, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
				% Bound to 5
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 14, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 15}, true)),
				assertion(agent_action(15, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(get_action(15, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID})),
		     	assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 16}, true)),
		     	assertion(agent_action(16, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID})),
		     	assertion(get_action(16, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID}))
		    )
		).

	test(image_score_lazy_actions):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		forall((strategy("Image Scoring Discriminator", _, [Options]), member("lazy", Options)),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [Options], community: ID, generation: 0, player: PlayerID}, true),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(agent_action(2, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(agent_action(3, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:idle}))
			)
		).

	test(image_score_promote_self_action):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		forall((strategy("Image Scoring Discriminator", _, [Options]), member("lazy", Options)),
			(
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [Options], community: ID, generation: 0, player: PlayerID}, true),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:gossip, value:positive: about: PlayerID, recipient:CoopID})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:gossip, value:positive: about: PlayerID, recipient:CoopID})),
		     	assertion(agent_action(2, ID, 0, PlayerID, true, action{type:gossip, value:positive: about: PlayerID, recipient:CoopID})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:gossip, value:positive: about: PlayerID, recipient:CoopID})),
		     	assertion(agent_action(3, ID, 0, PlayerID, true, action{type:gossip, value:positive: about: PlayerID, recipient:CoopID})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:gossip, value:positive: about: PlayerID, recipient:CoopID}))
			)
		).

	test(image_score_spread_accurate_positive):-
		forall((strategy("Image Scoring Discriminator", _, [1|OtherOptions]), member("spread_accurate_positive", OtherOptions)),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
				get_new_player_id(DefectID),
				new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [1|OtherOptions], community: ID, generation: 0, player: PlayerID}, true),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:idle})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 1, action: "cooperate"}, true),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: CoopID, recipient:DefectID})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: CoopID, recipient:DefectID})),
		     	assertion(agent_action(3, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: CoopID, recipient:DefectID})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: CoopID, recipient:DefectID})),
		     	assertion(agent_action(4, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: CoopID, recipient:DefectID})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: CoopID, recipient:DefectID})),
		     	add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 5, action: "defect"}, true),
		     	assertion(agent_action(6, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(6, ID, 0, PlayerID, true, action{type:idle})),
		     	add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 7, action: "cooperate"}, true),
		     	assertion(agent_action(8, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: DefectID, recipient:CoopID})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type:gossip, value:positive, about: DefectID, recipient:CoopID}))
			)
		).

	test(image_score_spread_accurate_negative):-
		forall((strategy("Image Scoring Discriminator", _, [1|OtherOptions]), member("spread_accurate_negative", OtherOptions)),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
				get_new_player_id(DefectID),
				new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
				get_new_player_id(PlayerID),
				new_agent(data{strategy: "Image Scoring Discriminator", options: [0|OtherOptions], community: ID, generation: 0, player: PlayerID}, true),
				assertion(agent_action(1, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(1, ID, 0, PlayerID, true, action{type:idle})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 1, action: "defect"}, true),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: CoopID, recipient:DefectID})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: CoopID, recipient:DefectID})),
		     	assertion(agent_action(3, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: CoopID, recipient:DefectID})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: CoopID, recipient:DefectID})),
		     	assertion(agent_action(4, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: CoopID, recipient:DefectID})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: CoopID, recipient:DefectID})),
		     	add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 5, action: "cooperate"}, true),
		     	assertion(agent_action(6, ID, 0, PlayerID, true, action{type:idle})),
		     	assertion(get_action(6, ID, 0, PlayerID, true, action{type:idle})),
		     	add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 7, action: "defect"}, true),
		     	assertion(agent_action(8, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: DefectID, recipient:CoopID})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type:gossip, value:negative, about: DefectID, recipient:CoopID}))
			)
		).



:- end_tests(discriminator).
