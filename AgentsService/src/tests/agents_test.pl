/** <module> This file runs testing on the logic in the agents file.
 * @author James King
 */

?- ['../communities'].
?- ['../agents'].
?- ['../strategies'].
:- use_module(library(debug)).
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

:- begin_tests(agents).

    test(new_agents_all_strategies):-
        new_community(ID),
        assertion(new_generation(data{community: ID, generation: 0}, true)),
        forall(strategy(Name, NonDonorStrat, TrustModel, _, Options), (get_new_player_id(PlayerID),
         assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true)))).

    test(new_agents_fail_on_retracted_community):-
        new_community(ID),
        assertion(retract_community(data{community: ID}, true)),
        assertion(new_generation(data{community: ID, generation: 0}, "No such community")),
        forall(strategy(Name, NonDonorStrat, TrustModel, _, Options), (get_new_player_id(PlayerID),
         assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, "No such community")))).

    test(new_agents_fail_on_retracted_community_post_new_generation):-
        new_community(ID),
        assertion(new_generation(data{community: ID, generation: 0}, true)),
        assertion(retract_community(data{community: ID}, true)),
        assertion(\+generation(community(ID), 0)),
        forall(strategy(Name, NonDonorStrat, TrustModel, _, Options), (get_new_player_id(PlayerID),
         assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, "No such community")))).

    test(new_agents_repeated_id):-
        new_community(ID),
        assertion(new_generation(data{community: ID, generation: 0}, true)),
        forall(strategy(Name, NonDonorStrat, TrustModel, _, Options), (get_new_player_id(PlayerID),
         (assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID}, true)) ;
         assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID},
          "Player ID already taken for this community and generation"))))).

    test(bad_input_dict):-
        new_community(ID),
        assertion(new_generation(data{community: ID, generation: 0}, true)),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model:"Trusting",  community: ID, generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", trust_model:"Trusting",  community: ID, generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy",  community: ID, generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{strategy: "Defector",  community: ID, generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model:"Trusting", options: ["lazy"], generation: 0, player: 0},
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model:"Trusting", options: ["lazy"], community: ID, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{community: ID, generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{ options: ["lazy"], generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{options: ["lazy"], community: ID, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{options: ["lazy"], community: ID, generation: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", community: ID, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", community: ID, generation: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", options: ["lazy"], player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", options: ["lazy"], generation: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", options: ["lazy"], community: ID}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{generation: 0, player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{donor_strategy: "Defector", options: ["lazy"]}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{options: ["lazy"], player: 0}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")),
        assertion(new_agent(data{}, 
         "Incorrect input, should contain the fields: donor_strategy, non_donor_strategy, trust_model, options, community, generation and player")).

    test(new_agents_incorrect_generation):-
        new_community(ID),
        forall(strategy(Name, NonDonorStrat, TrustModel, _, Options), (get_new_player_id(PlayerID),
         assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel, options: Options, community: ID, generation: 0, player: PlayerID},
          "No such generation for this community")))).

    test(new_agent_incorrect_strategy):-
        new_community(ID),
        assertion(new_generation(data{community: ID, generation: 0}, true)),
        assertion(new_agent(data{donor_strategy: "Hello", non_donor_strategy: "Hi", trust_model: "Hey",  options: [], community: ID, generation: 0, player: 0}, "No such strategy")),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Trusting", options: ["Incorrect"], community: ID, generation: 0, player: 0}, "No such strategy")),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Incorrect", trust_model: "Trusting", options: [], community: ID, generation: 0, player: 0}, "No such strategy")),
        assertion(new_agent(data{donor_strategy: "Defector", non_donor_strategy: "Lazy", trust_model: "Incorrect", options: [], community: ID, generation: 0, player: 0}, "No such strategy")).

    test(retract_agents):-
        new_community(ID),
        assertion(new_generation(data{community: ID, generation: 0}, true)),
        forall(strategy(Name, NonDonorStrat, TrustModel, _, Options), (get_new_player_id(PlayerID),
         assertion(new_agent(data{donor_strategy: Name, non_donor_strategy: NonDonorStrat, trust_model: TrustModel,  options: Options, community: ID, generation: 0, player: PlayerID}, true)))),
        retract_agents(ID),
        assertion(\+agent(_, community(ID), _, _)).


:- end_tests(agents).
