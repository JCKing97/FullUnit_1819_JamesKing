/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to agents strategies
--------------------------------------*/

:- module(strategies, [find_strategies//1]).

strategy( 'discriminator' , 'discriminates based on image score').
strategy( 'cooperator' , 'cooperates every time').
strategy( 'defector' , 'defects every time').

find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc}, strategy(Name, Desc), Strategies).
