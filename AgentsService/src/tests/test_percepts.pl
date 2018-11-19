/*--------------------------------------
Author:		James King
Title:		test_percepts.pl
Created:	4th Nov 2018
Desc:		Test the adding of percepts to an agents memory
--------------------------------------*/

:- begin_tests(percepts).
:- use_module('../strategies').
:- use_module('../communities').
?- ['../mvfcec/lib/utilities'].
?- ['../mvfcec/compiler/basic_V1.0'].
?- ['../mvfcec/lib/activity_recognition_lifecycles'].
?- ['../percepts'].

test(add_gossip):-
	communities:new_community(_),
	communities:new_generation(dict{community: 0, generation: 0}, _),
	strategies:new_agent(dict{community: 0, generation: 0, player: 1, strategy: defector}, _),
	strategies:new_agent(dict{community: 0, generation: 0, player: 0, strategy: defector}, _),
	strategies:new_agent(dict{community: 0, generation: 0, player: 2, strategy: defector}, _),
	add_percept(dict{perceiverId: 2, community: 0, generation: 0, timepoint:1, 
		percept: percept{type: "action", content: content{type: "gossip", about: 1, gossiper: 0, positive: @false}}}, Status4),
	Status4 == "Good",
	holds_at(bad_standing(strategies:agent(_, communities:community(0), communities:generation(communities:community(0), 0), 2),
		strategies:agent(_, communities:community(0), communities:generation(communities:community(0), 0), 1)), 1).
	
:- end_tests(percepts).