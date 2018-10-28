/*------------------------------------
Author:		James King
------------------------------------*/

:- module(random_saying, [random_saying_pred//0]).

random_saying_pred(List, Saying):-
	length(List, ListLen),
	ListLen = ListLen - 1,
	random_between(0, ListLen, RandNum),
	saying_looper(0, RandNum, List, Saying).

saying_looper(CurrentNum, RandNum, [Head|Tail], Saying):-
	(CurrentNum == RandNum -> Saying=Head ;
		(CurrentNum = CurrentNum + 1, saying_looper(CurrentNum+, RandNum, Tail, Saying))
	).
