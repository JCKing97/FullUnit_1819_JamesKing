/** <module> This file runs testing on the logic in the communities file.
 * @author James King
 */

?- ['../communities'].
?- ['../agents'].
:- use_module(library(debug)).
:- multifile community/1, generation/2.
:- dynamic community/1, generation/2.

:- begin_tests(communities, []).

	test(new_community, []):-
		new_community(ID0),
		new_community(ID1),
		new_community(ID2),
		assertion(communities:community(ID0)),
		assertion(communities:community(ID1)),
		assertion(communities:community(ID2)).

	test(create_then_retract_then_retract_communities, []):-	
		new_community(ID0),
		new_community(ID1),
		new_community(ID2),
		assertion(communities:community(ID0)),
		assertion(communities:community(ID1)),
		assertion(communities:community(ID2)),
		retractall(communities:community(_)),
		assertion(\+communities:community(ID0)),
		assertion(\+communities:community(ID1)),
		assertion(\+communities:community(ID2)),
		forall(\+communities:community(ID), assertion(retract_community(data{community: ID}, "No community with this ID to retract"))),
		assertion(\+communities:community(ID0)),
		assertion(\+communities:community(ID1)),
		assertion(\+communities:community(ID2)).

	test(retract_existing_community, []):-
		new_community(ID3),
		assertion(retract_community(data{community: ID3}, true)).

	test(retract_all_communities, []):-
		new_community(ID4),
		assertion(communities:community(ID4)),
		new_community(ID5),
		assertion(communities:community(ID5)),
		new_community(ID6),
		assertion(communities:community(ID6)),
		forall(community(ID), (writeln(ID))),
		forall(community(ID), (assertion(retract_community(data{community: ID}, true)))),
		findall(community(ID), community(ID), Communities1),
		writeln(Communities1),
		new_community(ID7),
		assertion(communities:community(ID7)).

	test(fill_gaps, []):-
		new_community(ID8),
		assertion(communities:community(ID8)),
		new_community(ID9),
		assertion(communities:community(ID9)),
		new_community(ID10),
		assertion(communities:community(ID10)),
		assertion(retract_community(data{community: ID9}, true)),
		assertion(retract_community(data{community: ID8}, true)),
		assertion(communities:community(ID10)),
		assertion(\+communities:community(ID9)),
		assertion(\+communities:community(ID8)),
		assertion(new_community(ID9)),
		assertion(communities:community(ID9)),
		assertion(new_community(ID8)),
		assertion(communities:community(ID8)),
		assertion(communities:community(ID10)).

:- end_tests(communities).
