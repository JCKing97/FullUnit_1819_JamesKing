/** <module> This file handles logging utilities for the http_log library
 * @author James King
*/

:- use_module(library(http/http_log)).

/**
 * log_list(++List:list) is det
 * 
 * Logs the passed List variable into the http logs.
 * @arg List The list that you wish to commit to logs.
 */
log_list([A]):-
	http_log('~w~n', [A]).
log_list([A|B]):-
	http_log('~w, ', [A]),
	log_list(B).