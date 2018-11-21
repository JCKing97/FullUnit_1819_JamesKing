/*
Author: James King
Date: Nov 2018
Tests for adding percepts to a player
*/

?- ['./loader'].

:- begin_tests(percepts).

test(experiment_standing, [setup(run(4))]):-
	assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 2), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 1))=bad, 2)),
	assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, ["distrusting"]), community(0), generation(community(0), 0), 3), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 2))=bad, 2)),
	assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, ["distrusting"]), community(0), generation(community(0), 0), 4), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 1))=bad, 3)),
	assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 0), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 1))=bad, 3)),
	assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, ["distrusting"]), community(0), generation(community(0), 0), 4), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 1))=good, 4)),
	assertion(\+holds_at(standing(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 2), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 0))=bad, 4)),
	assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 0), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 1))=good, 5)),
	assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 2), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 1))=good, 5)),
	assertion(holds_at(standing(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 2), agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 0))=bad, 6)).

test(experiments_general, [setup(run(4))]):-
	assertion(holds_at(last_donor_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=1, 2)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=1, 3)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=1, 4)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=4, 5)),
	assertion(\+holds_at(last_donor_timepoint(agent(strategy("defector", _, []), community(0), generation(community(0), 0), 6))=0, 1)),
	assertion(\+holds_at(last_donor_timepoint(agent(strategy("defector", _, []), community(0), generation(community(0), 0), 6))=0, 3)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("defector", _, []), community(0), generation(community(0), 0), 6))=3, 4)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("cooperator", _, []), community(0), generation(community(0), 0), 7))=5, 6)),
	assertion(\+holds_at(last_donor_timepoint(agent(strategy("cooperator", _, []), community(0), generation(community(0), 0), 7))=6, 6)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("cooperator", _, []), community(0), generation(community(0), 0), 7))=6, 7)),
	assertion(\+holds_at(last_donor_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 8))=0, 6)),
	assertion(holds_at(last_donor_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 8))=7, 8)),
	assertion(\+holds_at(last_recipient_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=0, 1)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=6, 7)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 5))=7, 8)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("defector", _, []), community(0), generation(community(0), 0), 6))=4, 5)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("defector", _, []), community(0), generation(community(0), 0), 6))=5, 6)),
	assertion(\+holds_at(last_recipient_timepoint(agent(strategy("cooperator", _, []), community(0), generation(community(0), 0), 7))=0, 1)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("cooperator", _, []), community(0), generation(community(0), 0), 7))=2, 3)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("cooperator", _, []), community(0), generation(community(0), 0), 7))=3, 4)),
	assertion(holds_at(last_recipient_timepoint(agent(strategy("Standing Discriminator", _, ["trusting"]), community(0), generation(community(0), 0), 8))=1, 2)).

:- end_tests(percepts).