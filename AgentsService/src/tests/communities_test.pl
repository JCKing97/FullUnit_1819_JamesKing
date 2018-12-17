/** <module> This file runs testing on the logic in the communities file.
 * @author James King
 */

?- ['../communities'].
?- ['../agents'].
:- use_module(library(debug)).

:- begin_tests(communities).

	test(new_community, []):-
		writeln("new_community"),
		new_community(ID0),
		new_community(ID1),
		new_community(ID2),
		assertion(community(ID0)),
		assertion(community(ID1)),
		assertion(community(ID2)).

	test(create_then_retract_then_retract_communities, []):-
		writeln("create_then_retract_then_retract_communities"),	
		new_community(ID11),
		new_community(ID12),
		new_community(ID13),
		assertion(community(ID11)),
		assertion(community(ID12)),
		assertion(community(ID13)),
		assertion(\+retract_community(data{community: ID11}, "No community with this ID to retract")),
		assertion(\+retract_community(data{community: ID12}, "No community with this ID to retract")),
		assertion(\+retract_community(data{community: ID13}, "No community with this ID to retract")),
		assertion(\+community(ID11)),
		assertion(\+community(ID12)),
		assertion(\+community(ID13)),
		forall(\+community(ID), writeln(ID)),
		assertion(retract_community(data{community: ID11}, "No community with this ID to retract")),
		assertion(retract_community(data{community: ID12}, "No community with this ID to retract")),
		assertion(retract_community(data{community: ID13}, "No community with this ID to retract")),
		assertion(\+community(ID11)),
		assertion(\+community(ID12)),
		assertion(\+community(ID13)).

	test(retract_existing_community, []):-
		writeln("retract_existing_community"),
		new_community(ID3),
		writeln(ID3),
		assertion(retract_community(data{community: ID3}, true)).

	test(retract_all_communities, []):-
		writeln("retract_all_communities"),
		new_community(ID4),
		new_community(ID5),
		new_community(ID6),
		assertion(community(ID4)),
		assertion(community(ID5)),
		assertion(community(ID6)),
		forall(community(ID), (writeln(ID))),
		forall(community(ID), (assertion(retract_community(data{community: ID}, true)))),
		forall(community(ID), (assertion(\+retract_community(data{community: ID}, "No community with this ID to retract")))),
		findall(community(ID), community(ID), Communities1),
		writeln(Communities1),
		new_community(ID7),
		assertion(community(ID7)).

	test(fill_gaps, []):-
		writeln("fill_gaps"),
		new_community(ID8),
		assertion(community(ID8)),
		new_community(ID9),
		assertion(community(ID9)),
		new_community(ID10),
		assertion(community(ID10)),
		assertion(retract_community(data{community: ID9}, true)),
		assertion(retract_community(data{community: ID8}, true)),
		assertion(community(ID10)),
		assertion(\+community(ID9)),
		assertion(\+community(ID8)),
		assertion(new_community(ID9)),
		assertion(community(ID9)),
		assertion(new_community(ID8)),
		assertion(community(ID8)),
		assertion(community(ID10)).

:- end_tests(communities).


:- begin_tests(generations).

	test(new_generation):-
		new_community(ID0),
		new_generation(data{community: ID0, generation: 0}, true).

	test(new_generations_one_community):-
		new_community(ID0),
		new_generation(data{community: ID0, generation: 0}, true),
		new_generation(data{community: ID0, generation: 1}, true),
		new_generation(data{community: ID0, generation: 2}, true).

	test(new_generation_multiple_communities):-
		new_community(_),
		new_community(_),
		new_community(_),
		forall(community(ID), assertion(new_generation(data{community: ID, generation: 0}, true) ;
		 new_generation(data{community: ID, generation: 0}, "This community already has a generation with this id"))).

	test(new_generations_multiple_communities):-
		new_community(_),
		new_community(_),
		new_community(_),
		forall(community(ID), assertion(new_generation(data{community: ID, generation: 0}, true) ;
		 new_generation(data{community: ID, generation: 0}, "This community already has a generation with this id"))),
		forall(community(ID), assertion(new_generation(data{community: ID, generation: 1}, true) ;
		 new_generation(data{community: ID, generation: 1}, "This community already has a generation with this id"))),
		forall(community(ID), assertion(new_generation(data{community: ID, generation: 2}, true) ; 
		 new_generation(data{community: ID, generation: 2}, "This community already has a generation with this id"))).

	test(retract_community_new_community_new_generations):-
		new_community(ID),
		assertion(new_generation(data{community: ID, generation: 0}, true)),
		assertion(new_generation(data{community: ID, generation: 0}, "This community already has a generation with this id")),
		assertion(retract_community(data{community: ID}, true)),
		assertion(new_generation(data{community: ID, generation: 0}, "No such community")),
		new_community(ID),
		assertion(new_generation(data{community: ID, generation: 0}, true)).

	test(new_generation_no_community_dict_entry):-
		assertion(new_generation(data{generation: 0}, "Incorrect input, no community field")).
	
	test(new_generation_no_generation_dict_entry):-
		new_community(ID),
		assertion(new_generation(data{community: ID}, "Incorrect input, no generation field")).

	test(new_generation_incorrect_community):-
		new_community(ID),
		assertion(retract_community(data{community: ID}, true)),
		assertion(new_generation(data{community: ID, generation: 0}, "No such community")).

	test(retract_generations_from_community):-
		new_community(ID),
		assertion(new_generation(data{community: ID, generation: 0}, true)),
		assertion(new_generation(data{community: ID, generation: 1}, true)),
		assertion(new_generation(data{community: ID, generation: 2}, true)),
		assertion(generation(community(ID), 0)),
		assertion(generation(community(ID), 1)),
		assertion(generation(community(ID), 2)),
		assertion(retract_community(data{community: ID}, true)),
		assertion(\+generation(community(ID), 0)),
		assertion(\+generation(community(ID), 1)),
		assertion(\+generation(community(ID), 2)),
		new_community(ID),
		assertion(new_generation(data{community: ID, generation: 0}, true)),
		assertion(new_generation(data{community: ID, generation: 1}, true)),
		assertion(new_generation(data{community: ID, generation: 2}, true)),
		assertion(generation(community(ID), 0)),
		assertion(generation(community(ID), 1)),
		assertion(generation(community(ID), 2)).		

:- end_tests(generations).