/** <module> This file runs testing on the logic in the strategies file.
 * @author James King
 */

?- ['../strategies'].
:- use_module(library(lists)).
:- use_module(library(debug)).

% Check the last strategy
test_strategies([strategy{name: Name, description: Desc, options: Options}], FoundStrategies):-
	assertion(strategy(Name, Desc, Options)),
	assertion(\+member(strategy(Name, Desc, Options), FoundStrategies)).
test_strategies([strategy{name: Name, description: Desc, options: Options}|T], FoundStrategies):-
	assertion(strategy(Name, Desc, Options)),
	assertion(\+member(strategy(Name, Desc, Options), FoundStrategies)),
	append(FoundStrategies, [strategy(Name, Desc, Options)], NewFoundStrategies),
	test_strategies(T, NewFoundStrategies).

:- begin_tests(strategies).

	test(find_all_strategies, []):-
 		find_strategies(Strategies),
 		assertion(test_strategies(Strategies, [])).

:- end_tests(strategies).