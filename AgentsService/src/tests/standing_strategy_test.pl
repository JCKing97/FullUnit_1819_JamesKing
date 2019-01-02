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
		new_agent(data{community: ID, generation: 0, player: CoopID, strategy: "Cooperator", options: ["lazy"]}, true),
		get_new_player_id(DefectID),
		new_agent(data{community: ID, generation: 0, player: DefectID, strategy: "Defector", options: ["lazy"]}, true),
		forall(strategy("Standing Discriminator", _, Options),
			 (get_new_player_id(PlayerID),
		     new_agent(data{strategy: "Standing Discriminator", options: Options, community: ID, generation: 0, player: PlayerID}, true),
		     % Initially all players are not of bad standing
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
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
		     assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 2)),
		     assertion(get_standing_belief(ID, 0, 2, PlayerID, DefectID, true, bad)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 2)),
		     assertion(get_standing_belief(ID, 0, 2, PlayerID, CoopID, true, good)),
		     % CoopID can defect against DefectID without loss of standing as DefectID is bad
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 3, action: "defect"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), CoopID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID), "defect"), 3)),
		     assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 4)),
		     assertion(get_standing_belief(ID, 0, 4, PlayerID, DefectID, true, bad)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 4)),
		     assertion(get_standing_belief(ID, 0, 4, PlayerID, CoopID, true, good)),
		     % Cooperation restores the status of DefectID
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "cooperate"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), DefectID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID), "cooperate"), 5)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 6)),
		     assertion(get_standing_belief(ID, 0, 6, PlayerID, DefectID, true, good)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 6)),
		     assertion(get_standing_belief(ID, 0, 6, PlayerID, CoopID, true, good)),
		     % CoopID defects against DefectID, PlayerID observes it and gives CoopID a bad standing
		     assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			  perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 7, action: "defect"},
			  true)),
		     assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), CoopID),
			  agent(_, community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID), "defect"), 7)),
		     assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), DefectID))=bad, 8)),
		     assertion(get_standing_belief(ID, 0, 8, PlayerID, DefectID, true, good)),
		     assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, Options), community(ID), generation(community(ID), 0), PlayerID),
			  agent(_, community(ID), generation(community(ID), 0), CoopID))=bad, 8)),
		     assertion(get_standing_belief(ID, 0, 8, PlayerID, CoopID, true, bad)))
		).

	test(trusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["trusting", "lazy"]}, true),
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

	test(distrusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["distrusting", "lazy"]}, true),
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

	test(spread_accurate_positive_trusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["trusting", "spread_accurate_positive"]}, true),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 1, recipient: 0, timepoint: 17, action: "defect"}, true)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 0, recipient: 1, timepoint: 29, action: "defect"}, true)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 0, recipient: 2, timepoint: 43, action: "defect"}, true)),
		check_correct_action_timepoints([0, 3, 5, 9, 12, 14, 16], ID, 0, 2, action{type: gossip, value: positive, about: _, recipient: _}),
		check_correct_action_timepoints([18, 19, 23, 26, 27, 32, 34, 41], ID, 0, 2, action{type: gossip, value: positive, about: 0, recipient: _}),
		check_correct_action_timepoints([44, 52, 76, 119, 134], ID, 0, 2, action{type: idle}).

	test(spread_accurate_negative_trusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["trusting", "spread_accurate_negative"]}, true),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 1, recipient: 0, timepoint: 5, action: "defect"}, true)),
		check_correct_action_timepoints([0, 1, 2, 4], ID, 0, 2, action{type: idle}),
		check_correct_action_timepoints([6, 8, 11, 13, 14, 120], ID, 0, 2, action{type: gossip, value: negative, about: 1, recipient: _}).

	test(lazy_trusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true), 
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Standing Discriminator", options: ["trusting", "lazy"]}, true),
		check_correct_action_timepoints([0, 1, 2, 7, 9, 10, 11, 120], ID, 0, 0, action{type: idle}).

	test(promote_self_trusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["trusting", "promote_self"]}, true),
		check_correct_action_timepoints([1,3,6,7,8,10,15,19,27,39,44,58,73,121], ID, 0, 2, action{type: gossip, value: positive, about: 2, recipient: _}).

	test(spread_accurate_positive_distrusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["distrusting", "spread_accurate_positive"]}, true),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 1, recipient: 0, timepoint: 17, action: "defect"}, true)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 0, recipient: 1, timepoint: 29, action: "defect"}, true)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 0, recipient: 2, timepoint: 43, action: "defect"}, true)),
		check_correct_action_timepoints([0, 3, 5, 9, 12, 14, 16], ID, 0, 2, action{type: gossip, value: positive, about: _, recipient: _}),
		check_correct_action_timepoints([18, 19, 23, 26, 27, 32, 34, 41], ID, 0, 2, action{type: gossip, value: positive, about: 0, recipient: _}),
		check_correct_action_timepoints([44, 52, 76, 119, 134], ID, 0, 2, action{type: idle}).

	test(spread_accurate_negative_distrusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["distrusting", "spread_accurate_negative"]}, true),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
			perceiver: 2, donor: 1, recipient: 0, timepoint: 5, action: "defect"}, true)),
		check_correct_action_timepoints([6, 8, 11, 13, 14, 120], ID, 0, 2, action{type: gossip, value: negative, about: 1, recipient: _}),
		check_correct_action_timepoints([0, 1, 2, 4], ID, 0, 2, action{type: idle}).

	test(lazy_distrusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Standing Discriminator", options: ["distrusting", "lazy"]}, true),
		check_correct_action_timepoints([0, 1, 2, 7, 9, 10, 11, 120], ID, 0, 0, action{type: idle}).

	test(promote_self_distrusting_standing):-
		new_community(ID),
		new_generation(data{community: ID, generation: 0}, true),
		new_agent(data{community: ID, generation: 0, player: 0, strategy: "Cooperator", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["lazy"]}, true),
		new_agent(data{community: ID, generation: 0, player: 2, strategy: "Standing Discriminator", options: ["distrusting", "promote_self"]}, true),
		check_correct_action_timepoints([1,4,6,8,9,10,12,14,23,33,41,59,75,132], ID, 0, 2, action{type: gossip, value: positive, about: 2, recipient: _}).


:- end_tests(standing_tests).