/** <module> This file tests the standing strategy
 * @author James King
 */

?- ['../percepts'].
?- ['../communities'].
?- ['../agents'].
?- ['../revise'].
?- ['../strategies'].
?- ['../beliefs'].

?-['../mvfcec/src/lib/utilities'].
?-['../mvfcec/src/compiler/basic_V1.0'].
?-['../mvfcec/src/lib/activity_recognition_lifecycles'].
dialect(swi).
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
         assertion(get_standing_belief(ID, 0, 8, PlayerID, CoopID, true, bad)))).

:- end_tests(standing_tests).