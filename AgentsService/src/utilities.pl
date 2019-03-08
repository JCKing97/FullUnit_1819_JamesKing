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
	length(List, Length),
	((Length @=< 0) -> fail ;
	(MaxIndex is Length-1,
	random_between(0, MaxIndex, Index),
	nth0(Index, List, Elem))).

/**
 * random_element(+List:list, -Elem:elem, +ElemNotIncl:elem) is nondet
 *
 * Get a random element from the list that is not ElemNotIncl, this element unifies with Elem.
 *
 * @arg List The list to get a random element from
 * @arg Elem The random element
 * @arg ElemNotIncl The element to not get from the list
 */

random_element_not_incl(List, Elem, ElemNotIncl):-
	delete(List, ElemNotIncl, ListWithout),
	length(ListWithout, Length),
	((Length @=< 0) -> fail ;
	(MaxIndex is Length-1,
	random_between(0, MaxIndex, Index),
	nth0(Index, ListWithout, Elem))).

/**
 * is_empty(+List:list, -IsIt:bool) is nondet
 *
 * Is the list input empty?
 * @arg List The list to check for emptiness
 * @arg IsIt Whether the list is empty or not
 */

is_empty(List, IsIt):-
	length(List, Length),
	((Length @=< 0) -> 
		IsIt = true ;
		IsIt = false
	).

/**
 * is_list_length_less_than(+List:list, +Val:int, -LessThan:bool)
 *
 * Is the list length less than the value Val.
 * @arg List The list to check
 * @arg Val The value to check it's length is less than
 * @arg LessThan Whether the length is less than Val or not
 */

is_list_length_less_than(List, Val, LessThan):-
	length(List, Length),
	((Length < Val) -> 
		LessThan = true ;
		LessThan = false
	).
