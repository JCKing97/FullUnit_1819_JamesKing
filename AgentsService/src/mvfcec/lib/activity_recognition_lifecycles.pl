%
% Author: Kostas Stathis
% Date: Oct 2015
%
% Lifecycle of activities
initiates_at(start(A), [], A=active, T):-
	happens_at(start(A), T).
initiates_at(suspend(A), [], A=suspended, T):-
	happens_at(suspend(A), T).
initiates_at(resume(A), [], A=active, T):-
	happens_at(resume(A), T).
initiates_at(interrupt(A), [], A=interrupted, T):-
	happens_at(interrupt(A), T).
initiates_at(complete(A), [], A=completed, T):-
	happens_at(complete(A), T).
	
% Lifecycle of goals
initiates_at(adopt(G), [], G=active, T):-
	happens_at(adopt(G), T).
initiates_at(deactivate(G), [],  G=deactivated, T):-
	happens_at(deactivate(G), T).
initiates_at(reactivate(G), [],  G=active, T):-
	happens_at(reactivate(G), T).
initiates_at(drop(G), [],  G=dropped, T):-
	happens_at(drop(G), T).
initiates_at(achieve(G), [],  G=achieved, T):-
	happens_at(achieve(G), T).
