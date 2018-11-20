/*

    Author:  Kostas Stathis
    Title:   utilities.pl
    Created: Jan 2010
    Updated: Sep 2015
    Desc:    Utilities, including dependencies on a dialect, plus other reusable code.

*/

:- dynamic debug_level/2.

l_cautious_assert(G):- 
	(once(clause(G,_)) -> \+ call(G), assert(G); assert(G)).

l_list_atomconcat([H1,H2|T], Atom):-
    !, l_list_atomconcat_iterarator([H1,H2|T], '', Atom).
l_list_atomconcat(_, _):-
    l_writelist(['l_list_atomconcat/2:',' First argument must be a list of at least two atomic elements.']),
    fail.

l_list_atomconcat_iterarator([], Atom, Atom).
l_list_atomconcat_iterarator([H1|T], AtomSoFar, FinalAtom):-
    l_atomconcat(AtomSoFar, H1, NewAtomSoFar),!,
    l_list_atomconcat_iterarator(T, NewAtomSoFar, FinalAtom).


l_set_debug_level(X):-
    debug_level(X),!.
l_set_debug_level(X):-
    retract(debug_level(_)),
    assert(debug_level(X)).

l_reconsult_from_dir(Dir, File):-
    l_atomconcat(Dir, File, DirFile),
    l_reconsult(DirFile).

l_debug_msg(L, _):- 
	L =< 0,!.
l_debug_msg(Level, Msg):-
	debug_level(L),
	Level =< L,
	(Msg = [_|_] -> l_writelist(Msg); l_write(Msg)).

% add if not in kb, otherwise do nothing (dependencies on a dialect)
l_add(G):- (l_compiled_call(G) -> true; l_assert(G)).

% solve once
l_once(G):-
    once(G).

% ground terms
l_ground(G):-
    ground(G).

% asserting
l_assert(X):-
    assert(X).

l_member(X,Y):-
    member(X,Y).

l_reconsult(X):-
    reconsult(X).

% Implementing forall
%l_forall(A, B) :- dialect(tuprolog),!, \+ (call(A), \+ call(B)).
l_forall(A, B) :- dialect(swi), forall(A, B).

% It should work for both.
l_atomic(X):- atomic(X).

%implementing atomconcat
%l_atomconcat(A, B, C):- dialect(tuprolog), !, text_concat(A, B, C).
l_atomconcat(A, B, C):- dialect(swi), atom_concat(A, B, C).

%implementing call
%l_compiled_call(G):- dialect(tuprolog), !, call(G).
l_compiled_call(G):- dialect(swi), (predicate_property(G, dynamic) -> call(G); fail).

%numbers
l_number(X):- number(X).

% retracting
l_retract(X):- retract(X).

% a call
l_call(G):- call(G).

%writing
l_write(X):- write(X).

l_writelist([]).
l_writelist([H|T]):-
	l_write(H),!,
	l_writelist(T).

% from (A,B) to [A,B]
l_body2list((H, Rest), [H|T]):-
        !,
        l_body2list(Rest, T).
l_body2list(Term,[Term]).

l_set(L,S):- dialect(swi),!, list_to_set(L,S).

l_set(L,S):- 'set_'(L,S). % I have not tested it

'set_'([],[]).
'set_'([H|T],[H|Out]) :-
    \+ member(H,T),
    !,
    'set_'(T,Out).
'set_'([_H|T],Out):-
    'set_'(T,Out).
