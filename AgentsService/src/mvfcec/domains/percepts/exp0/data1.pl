/* 
Percept Example for any players
Date: 20 Nov 2018
*/

:- dynamic agents/4.

% Agent 5 is a donor at timepoints: 1 and 4. The recipients of those acions are Agent 6
observed_at(interaction(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5),
	agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6)), 1).
observed_at(interaction(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5),
	agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6)), 4).
% Agent 6 is a donor at timpoints: 3. The recipient of that action is 7
observed_at(interaction(agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6),
	agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7)), 3).
% Agent 7 is a donor at timepoints: 5 and 6. The recipients of those actions are agents 6 and 5 respectively
observed_at(interaction(agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7),
	agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6)), 5).
observed_at(interaction(agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7),
	agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5)), 6).
% Agent 8 is a donor at timepoints: 7. The recipient of that action is Agent 5.
observed_at(interaction(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 8),
	agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5)), 7).