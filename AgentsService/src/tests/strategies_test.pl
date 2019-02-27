/** <module> This file runs testing on the logic in the strategies file.
 * @author James King
 */

?- ['../strategies'].
:- use_module(library(lists)).
:- use_module(library(debug)).

% Check the last strategy
test_strategies([strategy{donor_strategy: DonorStrategy, non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, description: Desc, options: Options}], FoundStrategies):-
	assertion(strategy(DonorStrategy, NonDonorStrategy, TrustModel, Desc, Options)),
	assertion(\+member(strategy(DonorStrategy, NonDonorStrategy, TrustModel, Desc, Options), FoundStrategies)).
test_strategies([strategy{donor_strategy: DonorStrategy, non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, description: Desc, options: Options}|T], FoundStrategies):-
	assertion(strategy(DonorStrategy, NonDonorStrategy, TrustModel, Desc, Options)),
	assertion(\+member(strategy(DonorStrategy, NonDonorStrategy, TrustModel, Desc, Options), FoundStrategies)),
	append(FoundStrategies, [strategy(DonorStrategy, NonDonorStrategy, TrustModel, Desc, Options)], NewFoundStrategies),
	test_strategies(T, NewFoundStrategies).

:- begin_tests(strategies).

	test(find_all_strategies, []):-
 		find_strategies(Strategies),
 		assertion(test_strategies(Strategies, [])).

 	test(find_image_discriminator_strategies):-
 		assertion(strategy("Image Scoring Discriminator", _, _, _, [0|_])),
 		assertion(strategy("Image Scoring Discriminator", _, _, _, [2|_])),
 		assertion(strategy("Image Scoring Discriminator", _, _, _, [-2|_])),
 		assertion(\+strategy("Image Scoring Discriminator", _, _, _, [3|_])),
 		assertion(\+strategy("Image Scoring Discriminator", _, _, _, [-3|_])).

:- end_tests(strategies).