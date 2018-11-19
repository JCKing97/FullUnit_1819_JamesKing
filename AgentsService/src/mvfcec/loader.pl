% Load dependencies for experiments processor using
% activity recognition lifecycles.

?- ['./config'].
?- ['./lib/utilities'].
?- ['./compiler/basic_V1.0'].
%?- ['./compiler/full_V1.0'].
?- ['./lib/activity_recognition_lifecycles'].
?- ['./experiments_processor'].

/* Running of experiments takes place here      */
run(1):-
    run_experiment(promotion, exp0, [promo_rules]).
run(2):-
    run_experiment(diabetes, test1, [dd_rules_ec2]).
run(3):-
    run_experiment(diabetes, test2, [dd_rules_ec2]).
run(3):-
    run_experiment(diabetes, exp1, [dd_rules_ec2]).
