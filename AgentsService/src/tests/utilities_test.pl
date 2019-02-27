/** <module> This file runs testing on utilities for the system in utilities.pl file.
 * @author James King
 */

?- ['../utilities'].
:- use_module(library(lists)).

list([1,2,3,4]).
list(["hi","hello","world"]).
list([true, false, yes, no]).
list([1]).
list(["hi"]).
list([1,2]).
list([2,3,4]).

:- begin_tests(random_element).
	
	test(regular_lists):-
		forall(list(List),
			(assertion(random_element(List, Element)),
			assertion(member(List, Element)))).

	test(empty_list):-
		\+random_element([], _).

:- end_tests(random_element).

:- begin_tests(is_empty).
	
	test(empty_list):-
		assertion(is_empty([], true)).

	test(non_empty_list):-
		assertion(is_empty([1,2,3,4], false)),
		assertion(is_empty([1], false)).

:- end_tests(is_empty).

loop_through_elements_check_if_not_incl(_, []).
loop_through_elements_check_if_not_incl(List, [Element|Tail]):-
	forall(random_element_not_incl(List, RandomElement, Element),
		(
			writeln(RandomElement),
			assertion(RandomElement\==Element)
		)
	),
	loop_through_elements_check_if_not_incl(List, Tail).

:- begin_tests(random_element_not_included).

	test(regular_lists):-
		forall(list(List),
			(
				writeln(List),
				loop_through_elements_check_if_not_incl(List, List)
			)
		).

	test(empty_list):-
		\+random_element_not_incl([], _, _).

:- end_tests(random_element_not_included).

:- begin_tests(is_list_less_than_length).

	test(regular_lists):-
		Val = 2,
		forall(list(List),
			(
				length(List, Len),
				( Len < 2 ) ->
					assertion(is_list_length_less_than(List, Val, true));
					assertion(is_list_length_less_than(List, Val, false))
			)
		).

:- end_tests(is_list_less_than_length).