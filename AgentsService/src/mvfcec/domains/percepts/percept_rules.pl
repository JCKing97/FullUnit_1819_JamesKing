/*
Author: James King
Date: 20 Nov 2018
Contains the rules for adding percepts to an agent
*/

initiates_at(said(Gossiper, Perceiver, About, PositiveOrNot), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, PositiveOrNot), T),
	PositiveOrNot == false,
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).

initiates_at(said(Gossiper, Perceiver, About, PositiveOrNot), [], standing(Perceiver, About)=good, T):-
	happens_at(said(Gossiper, Perceiver, About, PositiveOrNot), T),
	PositiveOrNot == true,
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).

causes_at(said(_,_,_,_), _, _).