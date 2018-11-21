/*--------------------------------------
Author:		James King
Title:		test_communities.pl
Created:	4th Nov 2018
Desc:		Test the adding of percepts to an agents memory
--------------------------------------*/

:- begin_tests(comm_agent).
?- ['../communities'].
?- ['../strategies'].

test(new_community, [setup(new_community(ID))]):-
	assertion(community(ID)).

test(new_generation, [setup(new_generation(dict{community: 0, generation: 0}, Status))]):-
	assertion(Status == "Good"),
	assertion(generation(community(0), 0)).

test(new_agent0, [setup(new_agent(dict{community: 0, generation: 0, player:0, strategy: defector}, Status))]):-
	assertion(Status == "Good"),
	assertion(agent(strategy(defector, _), community(0), generation(community(0), 0), 0)).

test(new_agent1, [setup(new_agent(dict{community: 0, generation: 0, player:1, strategy: cooperator}, Status))]):-
	assertion(Status == "Good"),
	assertion(agent(strategy(cooperator, _), community(0), generation(community(0), 0), 1)).

:- end_tests(comm_agent).