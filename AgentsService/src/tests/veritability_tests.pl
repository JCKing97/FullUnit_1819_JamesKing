/** <module> This file tests the Veritability Discerner strategy
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

:- begin_tests(initial_tests).

	test(initial_trusters):-
		forall((strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), K =< 0),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(trusted(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _), 0)),
				assertion(trusted(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _), 5)),
				assertion(trusted(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_])	, community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _), 10))
			)
		).

	test(initial_distrusters):-
		forall((strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), K > 0),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(\+trusted(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0),
				 PlayerID), agent(_, community(ID), generation(community(ID), 0), _), 0)),
				assertion(\+trusted(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID),
				 agent(_, community(ID), generation(community(ID), 0), _), 5)),
				assertion(\+trusted(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID), 
					agent(_, community(ID), generation(community(ID), 0), _), 10))
			)
		).

:- end_tests(initial_tests).

:- begin_tests(general_revision_tests).

	test(increment_percept_counts):-
		forall(strategy("Veritability Discerner", NonDonorStrat, TrustModel, Desc, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				assertion(\+holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
 				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, Desc, [K]), community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), DefectID))=1, 2)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: CoopID, recipient: DefectID, timepoint: 2, action: "defect"}, true),
				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID),
				 agent(_, community(ID), generation(community(ID), 0), CoopID))=1, 3)),
				assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 3, action: "cooperate"}, true)),
				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, Desc, [K]), community(ID), generation(community(ID), 0), PlayerID),
				 agent(_, community(ID), generation(community(ID), 0), DefectID))=2, 4)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 4}, true),
				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID), 
					agent(_, community(ID), generation(community(ID), 0), DefectID))=3, 5)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 5}, true),
				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID),
				 agent(_, community(ID), generation(community(ID), 0), DefectID))=4, 6)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "positive", timepoint: 6}, true),
				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID), 
					agent(_, community(ID), generation(community(ID), 0), CoopID))=2, 7)),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "negative", timepoint: 7}, true),
				assertion(holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, TrustModel, _, [K|_]), community(ID), generation(community(ID), 0), PlayerID),
				 agent(_, community(ID), generation(community(ID), 0), CoopID))=3, 8))
			)
		).

:- end_tests(general_revision_tests).

:- begin_tests(strong_reactor_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", Desc, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: "Strong Reactor", options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				assertion(\+holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(\+trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), DefectID), 2)),
				( trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
					agent(_, community(ID), generation(community(ID), 0), CoopID), 2) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -40, 2)) ;
					assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= _, 2))
				),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 4) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -60, 4)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -2, 4))
				),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 4) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -40, 5)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 18, 5))
				),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 6}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 6) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -30, 7)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Strong Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 19, 7))
				)
			)
		).

:- end_tests(strong_reactor_tests).

:- begin_tests(balanced_reactor_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", Desc, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: "Balanced Reactor", options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				assertion(\+holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(\+trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), DefectID), 2)),
				( trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
					agent(_, community(ID), generation(community(ID), 0), CoopID), 2) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -20, 2)) ;
					assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= _, 2))
				),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 4) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -30, 4)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -1, 4))
				),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 4) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -10, 5)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 19, 5))
				),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 6}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 6) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 0, 7)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 20, 7))
				)
			)
		).

:- end_tests(balanced_reactor_tests).

:- begin_tests(forgiving_reactor_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", Desc, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: "Forgiving Reactor", options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				assertion(\+holds_at(percept_count(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", Desc, [K]), community(ID), generation(community(ID), 0), PlayerID), agent(_, community(ID), generation(community(ID), 0), _))=_, 0)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(\+trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
					agent(_, community(ID), generation(community(ID), 0), DefectID), 2)),
				( trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
					agent(_, community(ID), generation(community(ID), 0), CoopID), 2) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -20, 2)) ;
					assertion(\+holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= _, 2))
				),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 4) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -30, 4)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= -1, 4))
				),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 4) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 10, 5)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 39, 5))
				),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 6}, true),
				(	trusted(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID), 
						agent(_, community(ID), generation(community(ID), 0), CoopID), 6) ->
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 30, 7)) ;
					assertion(holds_at(veritability_rating(agent(strategy("Veritability Discerner", NonDonorStrat, "Forgiving Reactor", _, [K]), community(ID), generation(community(ID), 0), PlayerID),
						agent(_, community(ID), generation(community(ID), 0), DefectID))= 41, 7))
				)
			)
		).

:- end_tests(forgiving_reactor_tests).

:- begin_tests(donor_action_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", NonDonorStrat, "Balanced Reactor", _, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: NonDonorStrat, trust_model: "Balanced Reactor", options: [K], community: ID, generation: 0, player: PlayerID}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: CoopID, timepoint: 1}, true)),
				( K =< 0 -> 
					(
						assertion(agent_action(1, ID, 0, PlayerID, true, action{type:action, value: cooperate, recipient: CoopID, reason: "I don't know anything about the recipient, but I am giving them a chance"})),
		     			assertion(get_action(1, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: CoopID, reason: "I don't know anything about the recipient, but I am giving them a chance"}))
		     		) ;
					(
						assertion(agent_action(1, ID, 0, PlayerID, true, action{type:action, value: defect, recipient: CoopID, reason: "I don't know anything about the recipient, so I will protect myself and defect"})),
		     			assertion(get_action(1, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: CoopID, reason: "I don't know anything about the recipient, so I will protect myself and defect"}))
		     		)
		     	),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 2, action: "defect"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint: 3}, true)),
				assertion(agent_action(3, ID, 0, PlayerID, true, action{type:action, value: defect, recipient: DefectID, reason: "The recipient's actions haven't been worthy of cooperation"})),
		     	assertion(get_action(3, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID, reason: "The recipient's actions haven't been worthy of cooperation"})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 5, action: "cooperate"}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint:6}, true)),
				assertion(agent_action(6, ID, 0, PlayerID, true, action{type:action, value: cooperate, recipient: DefectID, reason: "The recipient has generally acted in a good manner"})),
		     	assertion(get_action(6, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID, reason: "The recipient has generally acted in a good manner"})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 7}, true),
				assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: PlayerID, recipient: DefectID, timepoint:8}, true)),
				( K > 0 -> 
					(
						assertion(agent_action(8, ID, 0, PlayerID, true, action{type:action, value: defect, recipient: DefectID, reason:"The recipient's actions haven't been worthy of cooperation"})),
		     			assertion(get_action(8, ID, 0, PlayerID, true, action{type:action, value:defect, recipient: DefectID, reason: "The recipient's actions haven't been worthy of cooperation"}))
					) ;
					(
						assertion(agent_action(8, ID, 0, PlayerID, true, action{type:action, value: cooperate, recipient: DefectID, reason:"The recipient has generally acted in a good manner"})),
		     			assertion(get_action(8, ID, 0, PlayerID, true, action{type:action, value:cooperate, recipient: DefectID, reason: "The recipient has generally acted in a good manner"}))
					)
				)
			)
		).


:- end_tests(donor_action_tests).


:- begin_tests(lazy_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", "Lazy", TrustModel, _, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: "Lazy", trust_model: TrustModel, options: [K], community: ID, generation: 0, player: PlayerID}, true),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 6}, true),
				assertion(agent_action(8, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type:idle, reason:"I only act when I have to"}))
		     )
		).

:- end_tests(lazy_tests).

:- begin_tests(promote_self_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", "Promote Self", TrustModel, _, [K]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: "Promote Self", trust_model: TrustModel, options: [K], community: ID, generation: 0, player: PlayerID}, true),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 6}, true),
				assertion(agent_action(8, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type: gossip, value: positive, about: PlayerID, recipient: _, 
					reason: "I promote myself because I want to encourage others to cooperate with me"}))
		     )
		).

:- end_tests(promote_self_tests).

:- begin_tests(spread_positive_trusted_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", "Spread Positive Trusted", "Balanced Reactor", _, [0]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: "Spread Positive Trusted", trust_model: "Balanced Reactor", options: [0], community: ID, generation: 0, player: PlayerID}, true),
				assertion(agent_action(0, ID, 0, PlayerID, true, action{type: gossip, value: positive, about:AboutID, recipient:RecipID, 
					reason: "I believe the agent this gossip is about is trustworthy and want to spread that belief to other agents I believe are trustworthy"})),
		     	assertion(get_action(0, ID, 0, PlayerID, true, action{type: gossip, value: positive, about:AboutID, recipient:RecipID, 
					reason: "I believe the agent this gossip is about is trustworthy and want to spread that belief to other agents I believe are trustworthy"})),
		     	((AboutID is DefectID, RecipID is CoopID) ; (AboutID is CoopID, RecipID is DefectID)),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type: idle, reason: "I know no two trustworthy agents to spread positive gossip to and about."})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type: idle, reason: "I know no two trustworthy agents to spread positive gossip to and about."})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type: idle, reason: "I know no two trustworthy agents to spread positive gossip to and about."})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type: idle, reason: "I know no two trustworthy agents to spread positive gossip to and about."})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type: idle, reason: "I know no two trustworthy agents to spread positive gossip to and about."})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type: idle, reason: "I know no two trustworthy agents to spread positive gossip to and about."})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "positive", timepoint: 6}, true),
				assertion(agent_action(8, ID, 0, PlayerID, true, action{type: gossip, value: positive, about:AboutID1, recipient:RecipID1, 
					reason: "I believe the agent this gossip is about is trustworthy and want to spread that belief to other agents I believe are trustworthy"})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type: gossip, value: positive, about:AboutID1, recipient:RecipID1, 
					reason: "I believe the agent this gossip is about is trustworthy and want to spread that belief to other agents I believe are trustworthy"})),
		     	((AboutID1 is DefectID, RecipID1 is CoopID) ; (AboutID1 is CoopID, RecipID1 is DefectID))
		     )
		).

:- end_tests(spread_positive_trusted_tests).

:- begin_tests(spread_negative_trusted_tests).

	test(timeline):-
		forall(strategy("Veritability Discerner", "Spread Negative Untrusted", "Balanced Reactor", _, [0]),
			(
				new_community(ID),
				new_generation(data{community: ID, generation: 0}, true),
				get_new_player_id(CoopID),
				new_agent(data{donor_strategy: "Cooperator", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: CoopID}, true),
				get_new_player_id(DefectID),
				new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: DefectID}, true),
				get_new_player_id(PlayerID),
				new_agent(data{donor_strategy: "Veritability Discerner", non_donor_strategy: "Spread Negative Untrusted", trust_model: "Balanced Reactor", options: [0], community: ID, generation: 0, player: PlayerID}, true),
				assertion(agent_action(0, ID, 0, PlayerID, true, action{type: idle, reason: "I do not know any untrustworthy agents to warn trustworthy agents about"})),
		     	assertion(get_action(0, ID, 0, PlayerID, true, action{type: idle, reason: "I do not know any untrustworthy agents to warn trustworthy agents about"})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 1, action: "defect"}, true),
				assertion(agent_action(2, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:DefectID, recipient:CoopID, 
					reason: "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
		     	assertion(get_action(2, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:DefectID, recipient:CoopID, 
					reason:  "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
				add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: DefectID, gossiper: CoopID, gossip: "negative", timepoint: 3}, true),
				assertion(agent_action(4, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:DefectID, recipient:CoopID, 
					reason: "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
		     	assertion(get_action(4, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:DefectID, recipient:CoopID, 
					reason:  "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
				add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 4, action: "cooperate"}, true),
				assertion(agent_action(5, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:DefectID, recipient:CoopID, 
					reason: "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
		     	assertion(get_action(5, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:DefectID, recipient:CoopID, 
					reason:  "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
		     	add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: PlayerID,
					about: CoopID, gossiper: DefectID, gossip: "negative", timepoint: 7}, true),
		     	assertion(agent_action(8, ID, 0, PlayerID, true, action{type: idle, reason: "I do not know any trustworthy agents to warn about untrustworthy agents"})),
		     	assertion(get_action(8, ID, 0, PlayerID, true, action{type: idle, reason: "I do not know any trustworthy agents to warn about untrustworthy agents"})),
		     	add_new_action_interaction_percept(data{community: ID, generation: 0,
					perceiver: PlayerID, donor: DefectID, recipient: CoopID, timepoint: 9, action: "cooperate"}, true),
				assertion(agent_action(10, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:CoopID, recipient:DefectID, 
					reason: "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"})),
		     	assertion(get_action(10, ID, 0, PlayerID, true, action{type: gossip, value: negative, about:CoopID, recipient:DefectID, 
					reason: "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"}))
		     )
		).

:- end_tests(spread_negative_trusted_tests).