/** <module> This file tests the standing strategy
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

:- begin_tests(standing_tests).

	test(standing_timeline):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		get_new_player_id(CoopID),
		new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
		get_new_player_id(DefectID),
		new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
		forall(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options),
			(get_new_player_id(PlayerID),
		     new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true),
		     % Initially all players are not of bad standing
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), _))=bad, 0)),
		     assertion(get_standing_belief(ID, 0, 0, PlayerID, DefectID, true, good)),
		     assertion(get_standing_belief(ID, 0, 0, PlayerID, CoopID, true, good)),
		     % DefectID defects against CoopID so gets bad standing
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), DefectID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID), "defect"), 1)),
		     assertion(holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 2)),
		     assertion(get_standing_belief(ID, 0, 2, PlayerID, DefectID, true, bad)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 2)),
		     assertion(get_standing_belief(ID, 0, 2, PlayerID, CoopID, true, good)),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 2}, true)),
		     assertion(agent_action(2, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID, reason: "I am defecting because I believe the recipient to have a bad standing"})),
		     assertion(get_action(2, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID, reason: "I am defecting because I believe the recipient to have a bad standing"})),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 3}, true)),
		     assertion(agent_action(3, ID, 0, PlayerID, true, action{type:action, value: cooperate, recipient: CoopID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(get_action(3, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     % CoopID can defect against DefectID without loss of standing as DefectID is bad
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 3, action: "defect"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), CoopID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID), "defect"), 3)),
		     assertion(holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 4)),
		     assertion(get_standing_belief(ID, 0, 4, PlayerID, DefectID, true, bad)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 4)),
		     assertion(get_standing_belief(ID, 0, 4, PlayerID, CoopID, true, good)),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 4}, true)),
		     assertion(agent_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID, reason: "I am defecting because I believe the recipient to have a bad standing"})),
		     assertion(get_action(4, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID, reason: "I am defecting because I believe the recipient to have a bad standing"})),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 5}, true)),
		     assertion(agent_action(5, ID, 0, PlayerID, true, action{type:action, value: cooperate, recipient: CoopID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(get_action(5, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     % Cooperation restores the status of DefectID
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "cooperate"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), DefectID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID), "cooperate"), 5)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 6)),
		     assertion(get_standing_belief(ID, 0, 6, PlayerID, DefectID, true, good)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 6)),
		     assertion(get_standing_belief(ID, 0, 6, PlayerID, CoopID, true, good)),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 6}, true)),
		     assertion(agent_action(6, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(get_action(6, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 7}, true)),
		     assertion(agent_action(7, ID, 0, PlayerID, true, action{type:action, value: cooperate, recipient: CoopID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(get_action(7, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     % CoopID defects against DefectID, PlayerID observes it and gives CoopID a bad standing
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 7, action: "defect"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), CoopID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID), "defect"), 7)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 8)),
		     assertion(get_standing_belief(ID, 0, 8, PlayerID, DefectID, true, good)),
		     assertion(holds_at(standing(agent(strategy("Standing Discriminator", NonDonorStrategy, TrustModel, _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 8)),
		     assertion(get_standing_belief(ID, 0, 8, PlayerID, CoopID, true, bad)),
			 assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 8}, true)),
		     assertion(agent_action(8, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(get_action(8, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID, reason: "I am cooperating because I believe the recipient to have a good standing"})),
		     assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 9}, true)),
		     assertion(agent_action(9, ID, 0, PlayerID, true, action{type:action, value: defect, recipient: CoopID, reason: "I am defecting because I believe the recipient to have a bad standing"})),
		     assertion(get_action(9, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID, reason: "I am defecting because I believe the recipient to have a bad standing"}))
		)).

	test(trusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 0}, true),
		new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 1}, true),
		new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: "Lazy", trust_model: "Trusting", options: [], community: ID, generation: 0, player: 2}, true),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 0)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 0)),
		% Initially trust all sources
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 1, gossiper: 0, gossip: "negative", timepoint: 8}, true)),
		assertion(holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 9)),
		assertion(holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 11)),
		% Only believe gossip from trusted sources
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 0, gossiper: 1, gossip: "negative", timepoint: 12}, true)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 13)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 19)),
		% Positive gossip can restore the standing of an agent
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 1, gossiper: 0, gossip: "positive", timepoint: 17}, true)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 18)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 26)),
		% With the standing restored a player can push negative information
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 0, gossiper: 1, gossip: "negative", timepoint: 19}, true)),
		assertion(holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 20)),
		assertion(holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 26)).

	test(naive_trusting_standing):-
		forall(strategy("Standing Discriminator", NonDonorStrategy, "Naive Trusting", _, Options),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: NonDonorStrategy, trust_model: "Naive Trusting", options: Options, community: ID, generation: 0, player: PlayerID}, true),
				assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "negative", timepoint: 8}, true)),
				assertion(holds_at(standing(agent(_, community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 9)),
				assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 12}, true)),
				assertion(holds_at(standing(agent(_, community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 13)),
				assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: DefectID, gossip: "positive", timepoint: 18}, true)),
				assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 19))
			)
		).

	test(distrusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 0}, true),
		new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 1}, true),
		new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: "Lazy", trust_model: "Distrusting", options: [], community: ID, generation: 0, player: 2}, true),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 0)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 0)),
		% Don't believe gossip
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 1, gossiper: 0, gossip: "negative", timepoint: 8}, true)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 8)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 11)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 0, gossiper: 1, gossip: "negative", timepoint: 12}, true)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 12)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 19)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 1, gossiper: 0, gossip: "positive", timepoint: 17}, true)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 17)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 1))=bad, 26)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 2,
		 about: 0, gossiper: 1, gossip: "negative", timepoint: 19}, true)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 19)),
		assertion(\+holds_at(standing(agent(_, community(ID), generation(community(ID), 0), 2),
			  agent(_, community(ID), generation(community(ID), 0), 0))=bad, 26)).

	test(spread_accurate_positive_standing):-
		forall(strategy("Standing Discriminator", "Spread Accurate Positive", TrustModel, _, Options),
			(
				new_community(ID),
				get_new_player_id(CoopID),
				get_new_player_id(DefectID),
				get_new_player_id(PlayerID),
				new_generation(data{community: ID, generation: 0}, true),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: "Spread Accurate Positive", trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 17, action: "defect"}, true)),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 29, action: "defect"}, true)),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: PlayerID, timepoint: 43, action: "defect"}, true)),
				check_correct_action_timepoints([0, 3, 5, 9, 12, 14, 16], ID, 0, PlayerID, action{type: gossip, value: positive, about: _, recipient: _, 
					reason: "I am spreading positive gossip because I believe the recipient and the agent it is about are good"}),
				check_correct_action_timepoints([18, 19, 23, 26, 27, 32, 34, 41, 44, 52, 76, 119, 134], ID, 0, PlayerID, action{type: idle, reason: "I know no two good agents to be the recipient and the target of positive gossip"})
			)
		).

	test(spread_accurate_negative_trusting_standing):-
		forall(strategy("Standing Discriminator", "Spread Accurate Negative", TrustModel, _, Options),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				get_new_player_id(DefectID),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: "Spread Accurate Negative", trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "defect"}, true)),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: PlayerID, timepoint: 121, action: "defect"}, true)),
				check_correct_action_timepoints([0, 1, 2, 4], ID, 0, PlayerID, action{type: idle, reason: "I know of no agents with bad standing to warn others about"}),
				check_correct_action_timepoints([6, 8, 11, 13, 14, 120], ID, 0, PlayerID, action{type: gossip, value: negative, about: DefectID, recipient: CoopID,
 					reason: "I believe the agent this gossip is about to be of bad standing, so I am spreading it to those I believe are of good standing"}),
				check_correct_action_timepoints([121, 130, 140, 201], ID, 0, PlayerID, action{type: idle, reason: "I know of no agents with good standing to warn of the agents with bad standing"})
			)
		).

	test(lazy_trusting_standing):-
		forall(strategy("Standing Discriminator", "Lazy", TrustModel, _, Options),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				get_new_player_id(DefectID),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: "Lazy", trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 17, action: "defect"}, true)),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 29, action: "defect"}, true)),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: PlayerID, timepoint: 43, action: "defect"}, true)),
				check_correct_action_timepoints([0, 1, 2, 7, 9, 10, 11, 17, 19, 28, 32, 44, 56, 78, 101, 120], ID, 0, PlayerID, action{type: idle, reason: "I only act when I have to"})
			)
		).

	test(promote_self_trusting_standing):-
		forall(strategy("Standing Discriminator", "Promote Self", TrustModel, _, Options),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				get_new_player_id(DefectID),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				new_agent(data{donor_strategy: "Standing Discriminator", non_donor_strategy: "Promote Self", trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true),
				check_correct_action_timepoints([1,3,6,7,8,10,15,19,27,39,44,58,73,121], ID, 0, PlayerID, action{type: gossip, value: positive, about: PlayerID, recipient: _, reason: "I actively promote myself as I wish to encourage others to cooperate with me"})
			)
		).


:- end_tests(standing_tests).
 