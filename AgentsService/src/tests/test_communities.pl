/*--------------------------------------
Author:		James King
Title:		test_communities.pl
Created:	4th Nov 2018
Desc:		Test the adding of percepts to an agents memory
--------------------------------------*/

:- begin_tests(comm_agent).
:- use_module('../communities').
:- use_module('../strategies').

test(new_community, [setup(communities:new_community(ID))]):-
	communities:community(ID).

test(new_generation, [setup(communities:new_generation(dict{community: 0, generation: 0}, Status))]):-
	Status == "Good",
	communities:generation(communities:community(0), 0).

test(new_agent0, [setup(strategies:new_agent(dict{community: 0, generation: 0, player:0, strategy: defector}, Status))]):-
	Status == "Good",
	strategies:agent(_, communities:community(0), communities:generation(communities:community(0), 0), 0).

test(new_agent1, [setup(strategies:new_agent(dict{community: 0, generation: 0, player:1, strategy: cooperator}, Status))]):-
	Status == "Good",
	strategies:agent(_, communities:community(0), communities:generation(communities:community(0), 0), 1).

:- end_tests(comm_agent).