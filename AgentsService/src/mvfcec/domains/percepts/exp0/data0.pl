/* 
Percept Example
Date: 20 Nov 2018
*/

observed_at(initially(standing(agent(2), agent(1))=good), 0).
observed_at(initially(standing(agent(2), agent(3))=good), 0).
observed_at(said(agent(1), agent(2), agent(3), false), 3).
observed_at(said(agent(1), agent(2), agent(3), true), 6).