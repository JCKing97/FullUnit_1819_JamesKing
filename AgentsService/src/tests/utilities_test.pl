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