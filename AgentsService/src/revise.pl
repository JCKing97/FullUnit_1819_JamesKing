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

initiates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	TrustModel=="Naive Trusting",
	Gossip == "negative".

% If Gossiper is trusted and Gossip is positive
% Perceiver gives About good standing
terminates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	TrustModel=="Trusting",
	Gossip == "positive",
	\+holds_at(standing(Perceiver, Gossiper)=bad, T).

terminates_at(said(Gossiper, Perceiver, About, Gossip), [], standing(Perceiver, About)=bad, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Standing Discriminator",
	TrustModel=="Naive Trusting",
	Gossip == "positive".


/*-----------------------------
-------- Image Scoring --------
-----------------------------*/

% If an image scoring agent perceives a defection, remove 1 from that agents image score, limited by -5
initiates_at(did(Donor, Perceiver, Recipient, Action), [], image_score(Perceiver, Donor)=NewImageScore, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy, _, _, Options, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	Action=="defect",
	(
		( Options = [_,"Personal Grievance"], Perceiver==Recipient ) ->
			Change = 2 ; Change = 1
	),
	( 
		holds_at(image_score(Perceiver, Donor)=OldImageScore, T) -> 
			(OldImageScore =< -5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore-Change) ;
			NewImageScore is -Change
	).

% If an image scoring agent perceives a cooperation, add 1 to their image score, limited by 5
initiates_at(did(Donor, Perceiver, Recipient, Action), [], image_score(Perceiver, Donor)=NewImageScore, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy, _, _, Options, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	Action=="cooperate",
	(
		( Options = [_,"Personal Grievance"], Perceiver==Recipient ) ->
			Change = 2 ; Change = 1
	),
	(
		holds_at(image_score(Perceiver, Donor)=OldImageScore, T) -> 
			(OldImageScore >= 5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore+Change) ;
			NewImageScore is Change
	).

% If a trusting image scoring agent perceives negative gossip about an agent, remove 1 from the image score of the agent the gossip is about, bound by -5
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], image_score(Perceiver, About)=NewImageScore, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, [K|_], Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	TrustModel=="Trusting",
	Gossip=="negative",
	(
		holds_at(image_score(Perceiver, Gossiper)=GossiperImageScore, T) ->
			GossiperImageScore 	>= K ; 0 >= K
	),
	( 
		holds_at(image_score(Perceiver, About)=OldImageScore, T) -> 
			(OldImageScore =< -5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore-1) ;
			NewImageScore is -1
	).

% If a trusting image scoring agent perceives negative gossip about an agent, remove 1 from the image score of the agent the gossip is about, bound by -5
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], image_score(Perceiver, About)=NewImageScore, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	TrustModel=="Naive Trusting",
	Gossip=="negative",
	( 
		holds_at(image_score(Perceiver, About)=OldImageScore, T) -> 
			(OldImageScore =< -5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore-1) ;
			NewImageScore is -1
	).

% If a trusting image scoring agent perceives positive gossip about an agent, add 1 to the image score of the agent the gossip is about, bound by 5
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], image_score(Perceiver, About)=NewImageScore, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, [K|_], Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	TrustModel=="Trusting",
	Gossip=="positive",
	(
		holds_at(image_score(Perceiver, Gossiper)=GossiperImageScore, T) ->
			GossiperImageScore 	>= K ; 0 >= K
	),
	(
		holds_at(image_score(Perceiver, About)=OldImageScore, T) -> 
			(OldImageScore >= 5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore+1) ;
			NewImageScore is 1
	).

% If a trusting image scoring agent perceives negative gossip about an agent, remove 1 from the image score of the agent the gossip is about, bound by -5
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], image_score(Perceiver, About)=NewImageScore, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy, _, TrustModel, _, Perceiver),
	DonorStrategy=="Image Scoring Discriminator",
	TrustModel=="Naive Trusting",
	Gossip=="positive",
	( 
		holds_at(image_score(Perceiver, About)=OldImageScore, T) -> 
			(OldImageScore >= 5 -> NewImageScore is OldImageScore ; NewImageScore is OldImageScore+1) ;
			NewImageScore is 1
	).

/*--------------------------------------
-------- Veritability Discerner --------
--------------------------------------*/

/**
 * trusted(++Perceiver:term, ++Subject:term, ++T:int) is det
 *
 * true if the perceiver is a veritability discerner and trusts the subject at timepoint T, else fails.
 * an agent is trusted if their veritability_rating/percept_count >= K (set into the agents strategy)
 *
 * @arg Perceiver The agent that has beliefs about the Subject
 * @arg Subject The agent that the Perceiver trusts or does not trust
 * @arg T The timepoint at which the Perceiver either does or does not trust the Subject
 */
 % If the Perceiver has a veritability_rating for the Subject
trusted(Perceiver, Subject, T):-
	get_strategy(DonorStrategy, _, _, [K], Perceiver),
	DonorStrategy=="Veritability Discerner",
	holds_at(percept_count(Perceiver, Subject)=PerceptCount, T),
	holds_at(veritability_rating(Perceiver, Subject)=Rating, T), !,
	Mean is Rating/PerceptCount,
	Mean >= K.
% If the Perceiver has no veritability_rating for the Subject
trusted(Perceiver, _, _):-
	get_strategy(DonorStrategy, _, _, [K], Perceiver),
	DonorStrategy=="Veritability Discerner",
	0 >= K.

% If a veritability discerner sees an agent as a donor increment the amount of percepts they have seen of the donor's
initiates_at(did(Donor, Perceiver, Recipient, Action), [], percept_count(Perceiver, Donor)=NewCount, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy,_, _, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	(
		holds_at(percept_count(Perceiver, Donor)=OldCount, T) ->
			(NewCount is OldCount + 1) ; NewCount is 1
	).

% If a veritability discerner perceives gossip increment the amount of percepts they have received of the agent the gossip is about
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], percept_count(Perceiver, About)=NewCount, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy,_, _, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	(
		holds_at(percept_count(Perceiver, About)=OldCount, T) ->
			(NewCount is OldCount + 1) ; NewCount is 1
	).

% If a veritability discerner sees a cooperation increase the veritability rating of the donor with the weight given by the trust model
initiates_at(did(Donor, Perceiver, Recipient, Action), [], veritability_rating(Perceiver, Donor)=NewRating, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy,_, TrustModel, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	Action=="cooperate",
	(TrustModel=="Forgiving Reactor" -> Weight is 2 ; Weight is 1),
	(
		holds_at(veritability_rating(Perceiver, Donor)=OldRating, T) -> 
			NewRating is (Weight*20)+OldRating ; NewRating is Weight*20
	).

% If a veritability discerner sees a defection and trusts the recipient, decrease the veritability rating of the donor by the weight
% given in the tust model
initiates_at(did(Donor, Perceiver, Recipient, Action), [], veritability_rating(Perceiver, Donor)=NewRating, T):-
	happens_at(did(Donor, Perceiver, Recipient, Action), T),
	get_strategy(DonorStrategy,_, TrustModel, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	Action=="defect",
	trusted(Perceiver, Recipient, T),
	(TrustModel=="Strong Reactor" -> Weight is 2 ; Weight is 1),
	(
		holds_at(veritability_rating(Perceiver, Donor)=OldRating, T) -> 
			NewRating is (-20*Weight)+OldRating ; NewRating is -20*Weight
	).

% If a veritability discerner receives positive gossip from a trusted source increase the veritability rating of the 
% agent the gossip is about by the weight given
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], veritability_rating(Perceiver, About)=NewRating, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy,_, TrustModel, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	Gossip=="positive",
	trusted(Perceiver, Gossiper, T),
	(TrustModel=="Forgiving Reactor" -> Weight is 2 ; Weight is 1),
	(
		holds_at(veritability_rating(Perceiver, About)=OldRating, T) -> 
			NewRating is (Weight*10)+OldRating ; NewRating is Weight*10
	).

% If a veritability discerner receive positive gossip from an untrusted source, increate the veritability rating
% by a lesser amount than from a trusted source, but but still by the weight given by the trust model
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], veritability_rating(Perceiver, About)=NewRating, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy,_, TrustModel, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	Gossip=="positive",
	(TrustModel=="Forgiving Reactor" -> Weight is 2 ; Weight is 1),
	(
		holds_at(veritability_rating(Perceiver, About)=OldRating, T) -> 
			NewRating is (Weight*1)+OldRating ; NewRating is Weight*1
	).

% If a veritability discerner perceives negative gossip from a trusted source decrease the veritability rating
% by the given weight 
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], veritability_rating(Perceiver, About)=NewRating, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy,_, TrustModel, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	Gossip=="negative",
	trusted(Perceiver, Gossiper, T),
	(TrustModel=="Strong Reactor" -> Weight is 2 ; Weight is 1),
	(
		holds_at(veritability_rating(Perceiver, About)=OldRating, T) -> 
			NewRating is (-10*Weight)+OldRating ; NewRating is -10*Weight
	).

% If a veritability discerner perceives negative gossip from an untrusted source decrease the veritability rating
% less than it would with a trusted source, but by the weight given
initiates_at(said(Gossiper, Perceiver, About, Gossip), [], veritability_rating(Perceiver, About)=NewRating, T):-
	happens_at(said(Gossiper, Perceiver, About, Gossip), T),
	get_strategy(DonorStrategy,_, TrustModel, _, Perceiver),
	DonorStrategy=="Veritability Discerner",
	Gossip=="negative",
	(TrustModel=="Strong Reactor" -> Weight is 2 ; Weight is 1),
	(
		holds_at(veritability_rating(Perceiver, About)=OldRating, T) -> 
			NewRating is (-1*Weight)+OldRating ; NewRating is -1*Weight
	).