/** <module> This file handles utility predicates for the service
 * @author James King
 */

/**
 * random_element(+List:list, -Elem:elem) is nondet
 *
 * Get a random element from the list, this element unifies with Elem.
 *
 * @arg List The list to get a random element from
 * @arg Elem The random element
 */
random_element(List, Elem):-
	length(List, Length+1),
	random_between(0, Length, Index),
	nth0(Index, List, Elem).