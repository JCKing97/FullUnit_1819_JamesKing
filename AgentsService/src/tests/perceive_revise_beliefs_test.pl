/** <module> This file tests the inputting of percepts to agents
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

set_up(ID):-
	new_community(ID),
	new_generation(data{community: ID, generation: 0}, true),
	new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 0}, true),
	new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 1}, true),
	new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Void", options: [], community: ID, generation: 0, player: 2}, true).

:- begin_tests(action_interaction_tests).

	test(cooperate_observed_by_defector):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 0, donor: 1, recipient: 2, timepoint: 0, action: "cooperate"},
		 true)),
		assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 2), "cooperate"), 0)).

	test(defect_observed_by_cooperator):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
		 true)),
		assertion(observed_at(did(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 0), "defect"), 7)).

	test(action_not_cooperate_or_defect):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "hello"},
		 "Incorrect action must either be defect or cooperate")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 0), "hello"), 7)).

	test(no_such_community):-
		set_up(ID),
		retract_community(ID, true),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
		 "No such community")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 0), "defect"), 7)).

	test(no_such_generation):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 1,
		 perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
		 "No such generation for this community")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 1), 1),
		 agent(_, community(ID), generation(community(ID), 1), 2),
		 agent(_, community(ID), generation(community(ID), 1), 0), "defect"), 7)).

	test(matching_donor_recipient):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 2, donor: 0, recipient: 0, timepoint: 7, action: "defect"},
		 "Incorrect IDs: Donor and Recipient are the same")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 0), "defect"), 7)).

	test(incorrect_ids):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 6, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
		 "No such agent for: perceiver")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 6),
		 agent(_, community(ID), generation(community(ID), 0), 0), "defect"), 7)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 0, donor: 11, recipient: 0, timepoint: 7, action: "defect"},
		 "No such agent for: donor")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 11),
		 agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 0), "defect"), 7)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 1, donor: 2, recipient: 17, timepoint: 7, action: "defect"},
		 "No such agent for: recipient")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 17), "defect"), 7)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 1, donor: 6, recipient: 17, timepoint: 7, action: "defect"},
		 "No such agent for: donor or recipient")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 6),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 17), "defect"), 7)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 19, donor: 2, recipient: 17, timepoint: 7, action: "defect"},
		 "No such agent for: perceiver or recipient")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 19),
		 agent(_, community(ID), generation(community(ID), 0), 17), "defect"), 7)),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 19, donor: 13, recipient: 17, timepoint: 7, action: "defect"},
		 "No such agent for: perceiver, donor or recipient")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), 13),
		 agent(_, community(ID), generation(community(ID), 0), 19),
		 agent(_, community(ID), generation(community(ID), 0), 17), "defect"), 7)).

	test(missing_parts_of_input_dict):-
		set_up(ID),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 0, donor: 2, recipient: 1, action: "defect"},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 2, donor: 1, timepoint: 2, action: "defect"},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 0, donor: 1, recipient: 2},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(add_new_action_interaction_percept(data{community: ID,
		 perceiver: 0, donor: 1, recipient: 2, timepoint: 8},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 perceiver: 0, action: "defect"},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(add_new_action_interaction_percept(data{community: ID, generation: 0,
		 timepoint: 8, action: "defect"},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(add_new_action_interaction_percept(data{},
		 "Incorrect input, should include: community, generation, perceiver, donor, recipient, timepoint, action")),
		assertion(\+observed_at(did(agent(_, community(ID), generation(community(ID), 0), _),
		 agent(_, community(ID), generation(community(ID), 0), _),
		 agent(_, community(ID), generation(community(ID), 0), _), _), _)).

:- end_tests(action_interaction_tests).

:- begin_tests(gossip_tests).

	test(positive):-
		set_up(ID),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 0,
		 about: 1, gossiper: 2, gossip: "positive", timepoint: 0}, true)),
		assertion(observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 1), "positive"), 0)).

	test(negative):-
		set_up(ID),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 1,
		 about: 0, gossiper: 2, gossip: "negative", timepoint: 0}, true)),
		assertion(observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 0), "negative"), 0)).

	test(incorrect_gossip):-
		set_up(ID),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 1,
		 about: 0, gossiper: 2, gossip: "hello", timepoint: 0},
		 "Incorrect gossip action should be either positive or negative")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 0), "hello"), 0)).

	test(incorrect_community):-
		set_up(ID),
		retract_community(ID, true),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0, perceiver: 1,
		 about: 0, gossiper: 2, gossip: "positive", timepoint: 0},
		 "No such community")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 0), "positive"), 0)).

	test(incorrect_generation):-
		set_up(ID),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 1, perceiver: 1,
		 about: 0, gossiper: 2, gossip: "positive", timepoint: 0},
		 "No such generation for this community")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 0), "positive"), 0)).

	test(incorrect_ids):-
		set_up(ID),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 6, gossiper: 1, about: 0, timepoint: 7, gossip: "positive"},
		 "No such agent for: perceiver")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 6),
		 agent(_, community(ID), generation(community(ID), 0), 0), "positive"), 7)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 0, gossiper: 11, about: 0, timepoint: 7, gossip: "negative"},
		 "No such agent for: gossiper")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 11),
		 agent(_, community(ID), generation(community(ID), 0), 0), "negative"), 7)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 1, gossiper: 2, about: 17, timepoint: 7, gossip: "positive"},
		 "No such agent for: about")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 17), "positive"), 7)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 1, gossiper: 6, about: 17, timepoint: 7, gossip: "negative"},
		 "No such agent for: gossiper or about")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 6),
		 agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 17), "negative"), 7)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 19, gossiper: 2, about: 17, timepoint: 7, gossip: "positive"},
		 "No such agent for: perceiver or about")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 2),
		 agent(_, community(ID), generation(community(ID), 0), 19),
		 agent(_, community(ID), generation(community(ID), 0), 17), "positive"), 7)),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 19, gossiper: 13, about: 17, timepoint: 7, gossip: "negative"},
		 "No such agent for: perceiver, gossiper or about")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), 13),
		 agent(_, community(ID), generation(community(ID), 0), 19),
		 agent(_, community(ID), generation(community(ID), 0), 17), "negative"), 7)).

	test(missing_parts_of_input_dict):-
		set_up(ID),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 0, gossiper: 2, about: 1, gossip: "positive"},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 2, gossiper: 1, timepoint: 2, gossip: "positive"},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 0, gossiper: 1, about: 2},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(add_new_action_gossip_percept(data{community: ID,
		 perceiver: 0, gossiper: 1, about: 2, timepoint: 8},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 perceiver: 0, gossip: "negative"},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(add_new_action_gossip_percept(data{community: ID, generation: 0,
		 timepoint: 8, gossip: "negative"},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(add_new_action_gossip_percept(data{},
		 "Incorrect input must contain: community, generation, perceiver, about, gossiper, timepoint, gossip")),
		assertion(\+observed_at(said(agent(_, community(ID), generation(community(ID), 0), _),
		 agent(_, community(ID), generation(community(ID), 0), _),
		 agent(_, community(ID), generation(community(ID), 0), _), _), _)).

:- end_tests(gossip_tests).

:- begin_tests(interaction_tests).

	test(regular):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 3}, true)),
		assertion(observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])).

	test(multiple):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 3}, true)),
		assertion(observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 5}, true)),
		assertion(observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 5)),
		assertion(get_donor_belief(ID, 0, 0, 5, true, [_{timepoints: [3, 5], recipient: 1}])),
		assertion(get_recipient_belief(ID, 0, 1, 5, true, [_{timepoints: [3, 5], donor: 0}])),
		assertion(get_interaction_belief(ID, 0, 5, 0, 1, true, [_{timepoints: [3, 5], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 7}, true)),
		assertion(observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 7)),
		assertion(get_donor_belief(ID, 0, 0, 7, true, [_{timepoints: [3, 5, 7], recipient: 1}])),
		assertion(get_recipient_belief(ID, 0, 1, 7, true, [_{timepoints: [3, 5, 7], donor: 0}])),
		assertion(get_interaction_belief(ID, 0, 7, 0, 1, true, [_{timepoints: [3, 5, 7], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 0, timepoint: 9}, true)),
		assertion(observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 1),
		 agent(_, community(ID), generation(community(ID), 0), 0)), 9)),
		assertion(get_donor_belief(ID, 0, 0, 9, true, [_{timepoints: [3, 5, 7], recipient: 1}])),
		assertion(get_recipient_belief(ID, 0, 0, 9, true, [_{timepoints: [9], donor: 1}])),
		assertion(get_donor_belief(ID, 0, 1, 9, true, [_{timepoints: [9], recipient: 0}])),
		assertion(get_recipient_belief(ID, 0, 1, 9, true, [_{timepoints: [3, 5, 7], donor: 0}])),
		assertion(get_interaction_belief(ID, 0, 9, 0, 1, true, [_{timepoints: [3, 5, 7], donor: 0, recipient: 1},
		 _{timepoints: [9], donor: 1, recipient: 0}])).

	test(no_such_community):-
		set_up(ID),
		retract_community(ID, true),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 3}, "No such community")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])).

	test(no_such_generation):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 1, donor: 0, recipient: 1, timepoint: 3}, "No such generation for this community")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 1), 0),
		 agent(_, community(ID), generation(community(ID), 1), 1)), 3)),
		assertion(\+get_donor_belief(ID, 1, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 1, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 1, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])).

	test(incorrect_ids):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 3, recipient: 4, timepoint: 3},
		 "No such recipient or donor for this community and generation")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 3), 3),
		 agent(_, community(ID), generation(community(ID), 0), 4)), 3)),
		assertion(\+get_donor_belief(ID, 0, 3, 3, true, [_{timepoints: [3], recipient: 4}])),
		assertion(\+get_recipient_belief(ID, 0, 4, 3, true,  [_{timepoints: [3], donor: 3}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 3, 4, true, [_{timepoints: [3], donor: 3, recipient: 4}])),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 4, timepoint: 3},
		 "No such recipient for this community and generation")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 0),
		 agent(_, community(ID), generation(community(ID), 0), 4)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 4}])),
		assertion(\+get_recipient_belief(ID, 0, 4, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 4, true, [_{timepoints: [3], donor: 0, recipient: 4}])),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 3, recipient: 1, timepoint: 3},
		 "No such donor for this community and generation")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 3, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 3}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 3, 1, true, [_{timepoints: [3], donor: 3, recipient: 1}])).

	test(incorrect_input):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{generation: 0, donor: 0, recipient: 1, timepoint: 3},
		 "Incorrect input, must contain: community, generation, donor, recipient, timepoint")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{community: ID, recipient: 1, timepoint: 3},
		 "Incorrect input, must contain: community, generation, donor, recipient, timepoint")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0},
		 "Incorrect input, must contain: community, generation, donor, recipient, timepoint")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{community: ID, timepoint: 3},
		 "Incorrect input, must contain: community, generation, donor, recipient, timepoint")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])),
		assertion(add_new_interaction_percept(data{},
		 "Incorrect input, must contain: community, generation, donor, recipient, timepoint")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 1)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 1}])),
		assertion(\+get_recipient_belief(ID, 0, 1, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 1, true, [_{timepoints: [3], donor: 0, recipient: 1}])).

	test(matching_donor_recipient):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 0, timepoint: 3},
		 "Donor should not be the same as the recipient")),
		assertion(\+observed_at(interaction(agent(_, community(ID), generation(community(ID), 0), 3),
		 agent(_, community(ID), generation(community(ID), 0), 0)), 3)),
		assertion(\+get_donor_belief(ID, 0, 0, 3, true, [_{timepoints: [3], recipient: 0}])),
		assertion(\+get_recipient_belief(ID, 0, 0, 3, true,  [_{timepoints: [3], donor: 0}])),
		assertion(\+get_interaction_belief(ID, 0, 3, 0, 0, true, [_{timepoints: [3], donor: 0, recipient: 0}])).



:- end_tests(interaction_tests).

check_all_correct([Response]):-
	Percept = Response.percept,
	Success = Response.success,
	Type = Percept.type,
	( (Type == "action/gossip" ; Type == "action/interaction") ->
		(add_new_action_gossip_percept(Percept, true) -> 
			assertion(Success == true);
			(add_new_action_interaction_percept(Percept, true) -> 
				assertion(Success == true);
				assertion(Success \== true)
			)
		) ;
		assertion(Success == "Percept type incorrect, should be either action/interaction or action/gossip")
	), !.
check_all_correct([Response|OtherResponses]):-
	Percept = Response.percept,
	Success = Response.success,
	Type = Percept.type,
	( (Type == "action/gossip" ; Type == "action/interaction") ->
		(add_new_action_gossip_percept(Percept, true) -> 
			assertion(Success == true);
			(add_new_action_interaction_percept(Percept, true) -> 
				assertion(Success == true);
				assertion(Success \== true)
			)
		) ;
		assertion(Success == "Percept type incorrect, should be either action/interaction or action/gossip")
	),
	check_all_correct(OtherResponses).

:- begin_tests(group_tests).

	test(simple):-
		set_up(ID),
		Percepts = [
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 0, donor: 1, recipient: 2, timepoint: 0, action: "cooperate"},
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 0, about: 1, gossiper: 2, gossip: "positive", timepoint: 0},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 1, about: 0, gossiper: 2, gossip: "negative", timepoint: 0}
			],
		add_percepts(Percepts, SuccessList),
		check_all_correct(SuccessList).

	test(some_incorrect_percepts):-
		set_up(ID),
		Percepts = [
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 0, donor: 1, recipient: 2, timepoint: 0, action: "cooperate"},
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "hello"},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 0, about: 1, gossiper: 2, gossip: "positive", timepoint: 0},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 1, about: 0, gossiper: 2, gossip: "negative", timepoint: 0},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 0, donor: 2, recipient: 1, action: "defect"}
		],
		add_percepts(Percepts, SuccessList),
		check_all_correct(SuccessList).

	test(incorrect_types):-
		set_up(ID),
		Percepts = [
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 0, donor: 1, recipient: 2, timepoint: 0, action: "cooperate"},
			data{type: "incorrect/interaction", community: ID, generation: 0, perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "defect"},
			data{type: "action/interaction", community: ID, generation: 0, perceiver: 2, donor: 1, recipient: 0, timepoint: 7, action: "hello"},
			data{type: "action/incorrect", community: ID, generation: 0, perceiver: 0, about: 1, gossiper: 2, gossip: "positive", timepoint: 0},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 1, about: 0, gossiper: 2, gossip: "negative", timepoint: 0},
			data{type: "action/gossip", community: ID, generation: 0, perceiver: 0, donor: 2, recipient: 1, action: "defect"}
			],
		add_percepts(Percepts, SuccessList),
		check_all_correct(SuccessList).

:- end_tests(group_tests).