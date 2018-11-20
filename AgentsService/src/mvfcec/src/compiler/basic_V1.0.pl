/*
	Author:  Kostas Stathis
	Name:    basicMVFCEC.pl
	Created: Sep 2011
	Updated: Dec 2015

	Desc: 	Basic multi-value fluent CEC compiler - adapted from Chittaro and 
		Montanari original CEC paper, and extended here for multi-valued 
		fluents of Artikis etal under a weak interpretation of initiates_at 
		& terminates_at. MVFCEC has an extended ontology from that of Artikis 
		etal based on events causing derived events using a causes_at/3 
		predicate. It also relies on a different representation for mvis 
		from that originally developed CEC. Compiler assumes that input data 
		are experimental observations.
*/
:- dynamic observed_at/2, happens_at/2.
:- discontiguous initiates_at/4, terminates_at/4, holds_at/2.
:- multifile initiates_at/4, terminates_at/4.


initiates_at(initially(Prop), [], Prop, 0):-
	happens_at( initially(Prop), 0 ). 

holds_at(P, T) :-
	mholds_for(P, [Start, End]),
	gt(T, Start),
	le(T, End).

mholds_for(F=V, [Start, End]):-
	compiled_mvi_form_of(F=V, [Start, End], CompiledFluent),
	l_compiled_call(CompiledFluent).

% Transformation required to obtain the internal representation
% of fluents when they have been compiled.
compiled_mvi_form_of(F=V, I, CompiledFluent):-
	F =.. [Name|_],
	CompiledFluent =.. [Name, F=V, I].

% Retracting fluents in the internal representation.
retract_compiled_mvi_fluent(F=V, I):-
	compiled_mvi_form_of(F=V, I, CompiledFluent),
	retract(CompiledFluent).

% Asserting fluents in the internal representation.
assert_compiled_mvi_fluent(F=V, I, Mode):-
	assertion_mode(Mode),
	compiled_mvi_form_of(F=V, I, CompiledFluent),
	!,
	((Mode = cautious) 
	  -> 
	 l_cautious_assert(CompiledFluent);
	 assert(CompiledFluent)
        ).
assert_compiled_mvi_fluent(_, _, Mode):-
	l_writelist([Mode,' is not allowed! ','\n']),
	fail.

assertion_mode(default).
assertion_mode(cautious).

% We only support weak interpretation of initiates_at
broken_during(P, [Start, End]):-
	terminates_at(_E, _, P, T),
	lt(Start, T),
	gt(End, T).

broken_before(P, T):-
	terminates_at(_E, _, P, T1 ),
	lt(T, T1).

breakingT(_, T2, [_, T2]) :- !.
breakingT(Prop, T, [T1, T2]) :-
	retract_compiled_mvi_fluent(Prop, [T1, T2]),
	assert_compiled_mvi_fluent(Prop, [T1, T], default),
  	propagateRetract( [T, T2], Prop ).

creatingI(Prop, T) :-
	newTermination(Prop, [T, NewEnd]),
	assert_compiled_mvi_fluent(Prop, [T, NewEnd], cautious),
  	propagateAssert([T, NewEnd], Prop).

insideLeftCloseInt(Prop, T, [T1, T2]) :-
  	mholds_for(Prop, [T1, T2]),
  	le(T1, T), 
  	lt(T, T2).
  
insideRightCloseInt(Prop, T, [T1, T2]) :-
  	mholds_for(Prop, [T1, T2]),
  	lt(T1, T), 
  	le(T, T2).
  
leftOverlap([T1, T2], P, [T3, T4]) :-
  	lt(T1, T4), 
  	le(T4, T2),
  	mholds_for(P, [T3, T4]).
  
newInitiation(Prop, [NewStart, T]) :-
  	initiates_at( _, _, Prop, NewStart),
  	lt(NewStart, T),
  	\+ broken_during(Prop, [NewStart, T]), !.
newInitiation(Prop, [infMin, T]) :-
  	\+ broken_during( Prop, [infMin, T] ).

newTermination(Prop, [T, NewEnd]) :-
  	terminates_at(_, _, Prop, NewEnd),
  	lt(T, NewEnd),
  	\+ broken_during(Prop, [T, NewEnd]), !.
newTermination(Prop, [T, infPlus]) :-
  	\+ broken_during(Prop, [T, infPlus]).

propagateAssert(_, _).

propagateRetract(_, _).

rightOverlap([T1, T2], P, [T3, T4]) :-
  	lt(T1, T3),
  	le(T3, T2),
  	mholds_for(P, [T3, T4]).

reviseLeftOverlap(Pd, [_, T4]) :-
  	terminates_at(_, _, Pd, T4), !.
reviseLeftOverlap(Pd, [T3, T4]) :-
  	T3 \== infMin,	
  	newTermination(Pd, [T3, NewEnd]), !,
  	retract_compiled_mvi_fluent(Pd, [T3, T4]),
  	assert_compiled_mvi_fluent(Pd, [T3, NewEnd], cautious),
  	((gt(T4, NewEnd)) 
	  -> 
	  propagateRetract([NewEnd, T4], Pd);
	  propagateAssert([T4, NewEnd], Pd) 
	).
reviseLeftOverlap(Pd, [infMin, T4]) :-
  	terminates_at(_, _, Pd, T5),
  	\+ broken_before(Pd, T5), !,
  	retract_compiled_mvi_fluent(Pd, [infMin, T4]),
  	assert_compiled_mvi_fluent(Pd, [infMin, T5], default),
  	((gt(T4, T5)) 
	  -> 
	  propagateRetract([T5, T4], Pd);
	  propagateAssert([T4, T5], Pd) 
	).
reviseLeftOverlap(Pd, [T3, T4]) :-
  	retract_compiled_mvi_fluent(Pd, [T3, T4]),
  	propagateRetract([T3, T4], Pd).
  
reviseRightOverlap(Pd, [T3, _]) :-
  	initiates_at( _, _, Pd, T3 ), !.
reviseRightOverlap(Pd, [T3, T4]) :-
  	T4 \== infPlus,		
  	newInitiation(Pd, [NewStart, T4]), !,
  	retract_compiled_mvi_fluent(Pd, [T3, T4]),
	% Here's problem with the 5th update of the example of the CEC paper 
  	assert_compiled_mvi_fluent(Pd, [NewStart, T4], cautious),
  	((lt(T3, NewStart)) 
	  -> 
	  propagateRetract([T3, NewStart], Pd);
	  propagateAssert([NewStart, T3], Pd) 
	).
reviseRightOverlap(Pd, [T3, infPlus]) :-
  	initiates_at(_, _, Pd, T5),
  	\+ broken_during(Pd, [T5, infPlus]), !,
  	retract_compiled_mvi_fluent(Pd, [T3, infPlus]),
  	assert_compiled_mvi_fluent(Pd, [T5, infPlus], cautious),
  	((lt(T3, T5)) 
	 -> 
	 propagateRetract([T3, T5], Pd);
	 propagateAssert([T5, T3], Pd) 
	).
reviseRightOverlap(Pd, [T3, T4]) :-
  	retract_compiled_mvi_fluent(Pd, [T3, T4]),
  	propagateRetract([T3, T4], Pd).

updateInit(E, T, Prop) :-
	initiates_at(E, _, Prop, T),
	(insideLeftCloseInt(Prop, T, [_T1, _T2])
	->
	true;
	creatingI(Prop, T)
	).

updateTermin(E, T, Prop) :-
	terminates_at(E, _, Prop, T),
	(insideRightCloseInt(Prop, T, [T1, T2] )
	->
	breakingT(Prop, T, [T1, T2]);
	true
	).

% Generic terminates for Multi-value Fluents following Artikis etal.
terminates_at(E, C, Fluent = V1, T) :-
	var(Fluent),
	initiates_at(E, C, Fluent = V2, T),
	holds_at(Fluent = V1, T),
	\+ (V1 = V2).	
terminates_at(E, C, Fluent = V1, T) :-
	\+ var(Fluent),
	initiates_at(E, C, Fluent = V2, T),
	\+ (V1 = V2).

% Updating with a list of events.
updates_at([], _):-!.
updates_at([E|Es], T):-
	update_at(E, T),
	!,
	updates_at(Es, T).

% The update predicate of mvfCEC with an event that maybe causing others.
% Here we update also with the caused events.
update_at(E, T) :-
	effects_at(E, T),
	findall(CE, causes_at(E, CE, T), CEs),
	updates_at(CEs, T).

%The effects of an event in the knowledge base.
effects_at(E, T):-
  	l_cautious_assert(happens_at( E, T )), 
  	bagof(P, (updateInit( E, T, P ) ; updateTermin( E, T, P )), _),
	!.
effects_at(_,_).

% Standard interval comparison predicates used in mvfCEC (from CEC).
% infMin and infPlus stand for - and + infinity respectively.

ge(A, A) :- !.
ge(A, B) :- gt(A, B), !.

le(A, A) :- !.
le(A, B) :- lt(A, B), !.

gt(A, A) :- !, fail.
gt(_, infMin) :- !.
gt(_, infPlus) :- !, fail.
gt(infPlus, _) :- !.
gt(infMin, _) :- !, fail.
gt(A, B) :- A > B.

lt(A, A) :- !, fail.
lt(_, infMin) :- !, fail.
lt(_, infPlus) :- !.
lt(infPlus, _) :- !, fail.
lt(infMin, _) :- !.
lt(A, B) :- A < B.
