/* ---------------------------------------------------
Author:  Kostas Stathis
Title:   config.pl 
Created: Jan 2010
Updated: 7 Nov 2015
Desc:    Configuration file for running an experiment using the mvfCEC.
----------------------------------------------------- */

% dialect(+Prolog).
% Prolog system to be used. Currently one of: [swi | tuprolog]

dialect(swi).


% input_format(+Format, -Event, -Time).
% The format of the input observations.
% - It can be changed, to reflect different data.
% - Input narrative cannot be of the form happens_at/2.
% - in the example below, input is in terms of observed_at/2 assertions.

:- dynamic observed_at/2.

input_format(observed_at(E, T), E, T).

% domains_dir(+TopDir).
% Top directory for domains experiments and their associated files.
% Currently relative to where mvfCEC engine is stored. Absolute paths 
% can also be used.

domains_dir('../domains/').

% experimental_data(+Domain, +Experiment, +FilePrefix, +From, +To).
% Assumes that a +Domain is a directory under the top /domains directory,
% with different sub-directories containing different experiments. Each
% experiment is a collection of files starting with +FilePrefix and
% indexed by +From to +To, where +From <= +To.

% Experiments with one file
experimental_data(
                    promotion,
                    exp0,
                    data,
                    0,
                    0
).
experimental_data(
                    diabetes,
                    test1,
                    data,
                    0,
                    0
).

% Experiment with two files
experimental_data(
                    diabetes,
                    test2,
                    data,
                    0,
                    1
).

% Experiment with 365 files
experimental_data(
                    diabetes,
                    exp1,
                    simulation_,
                    0,
                    364
).


% debug_level(+Level).
% 0 - no debug
% 1 - high level debug.
% 2 - mid level debug.
% 3 - low level debug.

debug_level(3).
