/** <module> This file runs testing on the logic related to actions, commitments and capabilities.
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
:- multifile strategy/3, agent_action/6.

% Define a strategy that goes against capability rules
strategy( "Capability" , "Ignores capabilities", []).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Capability", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	random_element([defect, cooperate], DefectVSCoop),
	findall(RecipientID, agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID), Recipients),
	random_element(Recipients, Recipient),
	Success = true, Action = action{type:action, value: DefectVSCoop, recipient: Recipient}, !.
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Capability", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	Success = true, Action = action{type:idle}, !.


% Set up a community
set_up(ID):-
	new_community(ID),
	new_generation(data{community: ID, generation: 0}, true),
	new_agent(data{community: ID, generation: 0, player: 0, strategy: "Defector", options: ["lazy"]}, true),
	new_agent(data{community: ID, generation: 0, player: 1, strategy: "Defector", options: ["promote_self"]}, true),
	new_agent(data{community: ID, generation: 0, player: 2, strategy: "Defector", options: ["spread_negative"]}, true),
	new_agent(data{community: ID, generation: 0, player: 3, strategy: "Cooperator", options: ["lazy"]}, true),
	new_agent(data{community: ID, generation: 0, player: 4, strategy: "Cooperator", options: ["promote_self"]}, true),
	new_agent(data{community: ID, generation: 0, player: 5, strategy: "Cooperator", options: ["spread_positive"]}, true),
	new_agent(data{community: ID, generation: 0, player: 6, strategy: "Capability", options: []}, true),
	new_agent(data{community: ID, generation: 0, player: 7, strategy: "Random", options: []}, true).

:- begin_tests(capabilities).

	test(single_donor_timepoint):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 3}, true)),
		% Test when a donor
		assertion(capable(3, ID, 0, 0, action{type:action, value: defect, recipient: 1})),
		assertion(capable(3, ID, 0, 0, action{type:action, value: cooperate, recipient: 1})),
		get_capabilities(3, ID, 0, 0, Capabilities),
		assertion(length(Capabilities, 2)),
		assertion(\+capable(3, ID, 0, 0, action{type:idle})),
		assertion(\+capable(3, ID, 0, 0, action{type:gossip, value: _, about: _, recipient: _})),
		assertion(\+capable(3, ID, 0, 0, action{type:action, value: hello, recipient: 1})),
		assertion(\+capable(3, ID, 0, 0, action{type:action, value: defect, recipient: 2})),
		% Test when not a donor
		assertion(\+capable(7, ID, 0, 0, action{type:action, value: cooperate, recipient: 1})),
		assertion(\+capable(1, ID, 0, 0, action{type:hello})),
		assertion(\+capable(5, ID, 0, 0, action{type:gossip, value: hello, recipient: 1})),
		forall(agent(_, ID, generation(community(ID), 0), AgentID), 
			(
				assertion(capable(9, ID, 0, 0, action{type:gossip, value: positive, about: AgentID, recipient: _})),
			 	assertion(capable(2, ID, 0, 0, action{type:gossip, value: negative, about: AgentID, recipient: _}))
			)
		),
		assertion(capable(7, ID, 0, 0, action{type:idle})).

	test(capability_ignoring_strategy):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 6, recipient: 1, timepoint: 3}, true)),
		assertion(\+capable(5, ID, 0, 6, action{type:action, value: _, recipient: _})),
		assertion(get_action(5, ID, 0, 6, "Agent attempted to pick a non-permitted action", action{type: action, value: _, recipient: _})),
		assertion(capable(3, ID, 0, 6, action{type:action, value: _, recipient: _})),
		assertion(get_action(3, ID, 0, 6, "Agent attempted to pick a non-permitted action", Action)),
		assertion(Action \== action{type:action, value: _, recipient: _}).
		

:- end_tests(capabilities).

:- begin_tests(defector_action).

	test(lazy_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 3}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 2, timepoint: 7}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 0, recipient: 1, timepoint: 100}, true)),
		% Test when a donor
		forall(capable(Timepoint, ID, 0, 0, action{type: action, value: defect, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 0, true, action{type: action, value: defect, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 0, true, action{type: action, value: defect, recipient: RecipientID}))
			)
		),
		% Test when not a donor
		assertion(agent_action(4, ID, 0, 0, true, action{type: idle})),
		assertion(get_action(4, ID, 0, 0, true, action{type: idle})),
		assertion(agent_action(120, ID, 0, 0, true, action{type: idle})),
		assertion(get_action(120, ID, 0, 0, true, action{type: idle})),
		assertion(agent_action(1, ID, 0, 0, true, action{type: idle})),
		assertion(get_action(1, ID, 0, 0, true, action{type: idle})),
		assertion(agent_action(19, ID, 0, 0, true, action{type: idle})),
		assertion(get_action(19, ID, 0, 0, true, action{type: idle})).


	test(spread_negative_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 2, recipient: 0, timepoint: 5}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 2, recipient: 1, timepoint: 9}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 2, recipient: 0, timepoint: 13}, true)),
		% Test when a donor
		forall(capable(Timepoint, ID, 0, 2, action{type: action, value: defect, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 2, true, action{type: action, value: defect, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 2, true, action{type: action, value: defect, recipient: RecipientID}))
			)
		),
		% Test when not a donor
		assertion(agent_action(4, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(get_action(4, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(agent_action(130, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(get_action(130, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(agent_action(2, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(get_action(2, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(agent_action(17, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})),
		assertion(get_action(17, ID, 0, 2, true, action{type: gossip, value: negative, about: _, recipient: _})).


	test(promote_self_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 2, timepoint: 14}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 2, timepoint: 9}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 0, timepoint: 130}, true)),
		% Test when a donor
		forall(capable(Timepoint, ID, 0, 1, action{type: action, value: defect, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 1, true, action{type: action, value: defect, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 1, true, action{type: action, value: defect, recipient: RecipientID}))
			)
		),
		% Test when not a donor
		assertion(agent_action(17, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(get_action(17, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(agent_action(131, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(get_action(131, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(agent_action(3, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(get_action(3, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(agent_action(13, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})),
		assertion(get_action(13, ID, 0, 1, true, action{type: gossip, value: positive, about: 1, recipient: _})).

:- end_tests(defector_action).

:- begin_tests(cooperator_action).

	test(lazy_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 3, recipient: 1, timepoint: 1}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 3, recipient: 4, timepoint: 19}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 3, recipient: 5, timepoint: 26}, true)),
		% Test when a donor
		forall(capable(Timepoint, ID, 0, 3, action{type: action, value: cooperate, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 3, true, action{type: action, value: cooperate, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 3, true, action{type: action, value: cooperate, recipient: RecipientID}))
			)
		),
		% Test when not a donor
		assertion(agent_action(2, ID, 0, 3, true, action{type: idle})),
		assertion(get_action(2, ID, 0, 3, true, action{type: idle})),
		assertion(agent_action(0, ID, 0, 3, true, action{type: idle})),
		assertion(get_action(0, ID, 0, 3, true, action{type: idle})),
		assertion(agent_action(20, ID, 0, 3, true, action{type: idle})),
		assertion(get_action(20, ID, 0, 3, true, action{type: idle})),
		assertion(agent_action(300, ID, 0, 3, true, action{type: idle})),
		assertion(get_action(300, ID, 0, 3, true, action{type: idle})).


	test(spread_positive_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 5, recipient: 0, timepoint: 2}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 5, recipient: 3, timepoint: 21}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 5, recipient: 4, timepoint: 210}, true)),
		% Test when a donor
		forall(capable(Timepoint, ID, 0, 5, action{type: action, value: cooperate, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 5, true, action{type: action, value: cooperate, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 5, true, action{type: action, value: cooperate, recipient: RecipientID}))
			)
		),
		% Test when not a donor
		assertion(agent_action(3, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(get_action(3, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(agent_action(102, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(get_action(102, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(agent_action(8, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(get_action(8, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(agent_action(17, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})),
		assertion(get_action(17, ID, 0, 5, true, action{type: gossip, value: positive, about: _, recipient: _})).


	test(promote_self_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 4, recipient: 2, timepoint: 6}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 4, recipient: 5, timepoint: 102}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 4, recipient: 0, timepoint: 209}, true)),
		% Test when a donor
		forall(capable(Timepoint, ID, 0, 4, action{type: action, value: cooperate, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 4, true, action{type: action, value: cooperate, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 4, true, action{type: action, value: cooperate, recipient: RecipientID}))
			)
		),
		% Test when not a donor
		assertion(agent_action(13, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(get_action(13, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(agent_action(7, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(get_action(7, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(agent_action(103, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(get_action(103, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(agent_action(14, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})),
		assertion(get_action(14, ID, 0, 4, true, action{type: gossip, value: positive, about: 4, recipient: _})).

:- end_tests(cooperator_action).

:- begin_tests(commitments).

	test(single):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 4, recipient: 2, timepoint: 6}, true)),
		% Using agent action predicate asserts no commitments
		assertion(agent_action(6, ID, 0, 4, true, action{type: action, value: cooperate, recipient: 2})),
		% Using get_action predicate asserts a commitment
		assertion(get_action(6, ID, 0, 4, true, action{type: action, value: cooperate, recipient: 2})),
		assertion(get_action(6, ID, 0, 4, "Agent has already committed to an action at this timepoint",
		 action{type: action, value: cooperate, recipient: 2})).

	test(multiple):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 2, timepoint: 14}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 2, timepoint: 9}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 1, recipient: 0, timepoint: 130}, true)),
		% Using agent action predicate asserts no commitments
		forall(capable(Timepoint, ID, 0, 1, action{type: action, value: defect, recipient: RecipientID}),
			(
				assertion(agent_action(Timepoint, ID, 0, 1, true, action{type: action, value: defect, recipient: RecipientID}))
			)
		),
		% Using get_action predicate asserts a commitment
		forall(capable(Timepoint, ID, 0, 1, action{type: action, value: defect, recipient: RecipientID}),
			(
				assertion(get_action(Timepoint, ID, 0, 1, true, action{type: action, value: defect, recipient: RecipientID})),
				assertion(get_action(Timepoint, ID, 0, 1, "Agent has already committed to an action at this timepoint",
				 action{type: action, value: defect, recipient: RecipientID}))
			)
		).


:- end_tests(commitments).

check_meets_capability_in_timepoints([Timepoint], CommunityID, GenerationID, AgentID):-
	assertion(agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action1)),
	assertion(get_action(Timepoint, CommunityID, GenerationID, AgentID, _, Action2)),
	assertion(capable(Timepoint, CommunityID, GenerationID, AgentID, Action1)),
	assertion(capable(Timepoint, CommunityID, GenerationID, AgentID, Action2)).
check_meets_capability_in_timepoints([Timepoint|OtherTimepoints], CommunityID, GenerationID, AgentID):-
	assertion(agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action1)),
	assertion(get_action(Timepoint, CommunityID, GenerationID, AgentID, _, Action2)),
	assertion(capable(Timepoint, CommunityID, GenerationID, AgentID, Action1)),
	assertion(capable(Timepoint, CommunityID, GenerationID, AgentID, Action2)),
	check_meets_capability_in_timepoints(OtherTimepoints, CommunityID, GenerationID, AgentID).


:- begin_tests(random_action).

	test(get_random_actions):-
		set_up(ID),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 7, recipient: 4, timepoint: 34}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 7, recipient: 1, timepoint: 67}, true)),
		assertion(add_new_interaction_percept(data{community: ID, generation: 0, donor: 7, recipient: 5, timepoint: 22}, true)),
		% Using agent action predicate asserts no commitments
		check_meets_capability_in_timepoints([1, 5, 8, 18, 22, 32, 34, 45, 63, 67, 71, 78, 128], ID, 0, 7).


:- end_tests(random_action).