/* 
Percept Example for any players
Date: 20 Nov 2018
*/

:- dynamic agents/4.

% Agent 0 is a donor at timepoints: 1 and 4
observed_at(donor(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5)), 1).
observed_at(donor(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5)), 4).
% Agent 1 is a donor at timpoints: 2 and 3
observed_at(donor(agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6)), 3).
% Agent 2 is a donor at timepoints: 5 and 6
observed_at(donor(agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7)), 5).
observed_at(donor(agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7)), 6).
% Agent 3 is a donor at timepoints: 7
observed_at(donor(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 8)), 7).
% Agent 0 is a recipient at timepoints: 6 and 7
observed_at(recipient(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5)), 6).
observed_at(recipient(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 5)), 7).
% Agent 1 is a recipient at timepoints: 4 and 5
observed_at(recipient(agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6)), 4).
observed_at(recipient(agent(strategy("defector", "desc", []), community(0), generation(community(0), 0), 6)), 5).
% Agent 2 is a recipient at timepoints: 2 and 3
observed_at(recipient(agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7)), 2).
observed_at(recipient(agent(strategy("cooperator", "desc", []), community(0), generation(community(0), 0), 7)), 3).
% Agent 3 is a recipient at timepoints: 1
observed_at(recipient(agent(strategy("Standing Discriminator", "desc", ["trusting"]), community(0), generation(community(0), 0), 8)), 1).
