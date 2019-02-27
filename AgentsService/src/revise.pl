/** <module> This file handles the revision of beliefs triggered by events being asserted in the event calculus.
 * @author James King
 */

:- use_module(library(lists)).
:- discontiguous initiates_at/4.
?- ['./agents'].
?- ['./strategies'].
?- ['./communities'].
% Compile and set up mvfcec
?- ['./mvfcec/src/lib/utilities'].
?- ['./mvfcec/src/compiler/basic_V1.0'].
?- ['./mvfcec/src/lib/activity_recognition_lifecycles'].
:- dynamic observed_at/2.

% Get the strategy from an agent
get_strategy(DonorStrategy, NonDonorStrategy, TrustModel, Options, agent(strategy(DonorStrategy, NonDonorStrategy, TrustModel, _, Options), _, _, _)).

/**
 * initiates_at(++Event:term, @Options:list, ?Fluent:term, --T:int) is nondet
 *
 * Initiate a certain value for a fluent at the given timepoint T, based on the event passed in Event.
 *
 * @arg Event The event that may have happened
 * @arg Options A list of options (keep empty for now)
 * @arg Fluent The fluent and value that will now hold if the event happened and logic is satisfies
 * @arg T The timepoint at which this fluent was initiated
 */

/*-----------------------------
----------- General -----------
-----------------------------*/

% Add to the interaction timepoints for when interactions occurred between two agents
initiates_at(interaction(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient)), [],
  interaction_timepoints(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient))=Timepoints, T):-
	happens_at(interaction(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient)), T),
	CheckTimepoint is T - 1,
	holds_at(interaction_timepoints(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient))=OldTimepoints, CheckTimepoint),
	append(OldTimepoints, [T], Timepoints).
initiates_at(interaction(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient)), [],
  interaction_timepoints(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient))=[T], T):-
	happens_at(interaction(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient)), T),
	CheckTimepoint is T - 1,
	\+holds_at(interaction_timepoints(agent(_, Community, Generation, Donor), agent(_, Community, Generation, Recipient))=_, CheckTimepoint).

causes_at(said(_, _,_,_), nothing, _).
causes_at(did(_, _,_,_), nothing, _).
causes_at(interaction(_, _), nothing, _).

/*-----------------------------
------ Standing Strategy ------
-----------------------------*/

% How actions affect the standing of a player
% If a donor defects against a recipient with good standing, give donor a bad standing to the perceiver
initiates_at(did(Donor, Perceiver, Recipient, Action), [], standing(Perceiver, Donor)=bad, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy, _, _, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	Action == "defect",
	\+holds_at(standing(Perceiver, Recipient)=bad, T).

% If a donor cooperates with anyone, give donor a good standing to the perceiver
terminates_at(did(Donor, Perceiver, Recipient, Action), [], standing(Perceiver, Donor)=bad, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy, _, _, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	Action=="cooperate".

% How gossip affects the standing of a player
% If Gossiper is trusted (Perceiver is a trusting agent and holds Gossiper in good standing)
% And Gossip is negative Perceiver gives About bad standing
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	TrustModel=="Trusting",
	Gossip == "negative",
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).

% If Gossiper is trusted and Gossip is positive
% Perceiver gives About good standing
terminates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	TrustModel="Trusting",
	Gossip == "positive",
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).


/*-----------------------------
-------- Image Scoring --------
-----------------------------*/

% If an image scoring agent perceives a defection, remove 1 from that agents image score, limited by -5
initiates_at(did(Donor, Perceiver, Recipient, Action), [], image_score(Perceiver, Donor)=NewImageScore, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy, _, _, _, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	Action=="defect",
	( 
		holds_at(image_score(Perceiver, Donor)=OldImageScore, T) -> 
			(OldImageScore =< -5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore-1) ;
			NewImageScore is -1
	).

% If an image scoring agent perceives a cooperation, add 1 to their image score, limited by 5
initiates_at(did(Donor, Perceiver, Recipient, Action), [], image_score(Perceiver, Donor)=NewImageScore, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy, _, _, _, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	Action=="cooperate",
	(
		holds_at(image_score(Perceiver, Donor)=OldImageScore, T) -> 
			(OldImageScore >= 5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore+1) ;
			NewImageScore is 1
	).

% If a trusting image scoring agent perceives negative gossip about an agent, remove 1 from the image score of the agent the gossip is about, bound by -5
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], image_score(Perceiver, About)=NewImageScore, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	TrustModel=="Trusting",
	Gossip=="negative",
	( 
		holds_at(image_score(Perceiver, About)=OldImageScore, T) -> 
			(OldImageScore =< -5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore-1) ;
			NewImageScore is -1
	).

% If a trusting image scoring agent perceives positive gossip about an agent, add 1 to the image score of the agent the gossip is about, bound by 5
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], image_score(Perceiver, About)=NewImageScore, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	TrustModel=="Trusting",
	Gossip=="positive",
	(
		holds_at(image_score(Perceiver, About)=OldImageScore, T) -> 
			(OldImageScore >= 5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore+1) ;
			NewImageScore is 1
	).