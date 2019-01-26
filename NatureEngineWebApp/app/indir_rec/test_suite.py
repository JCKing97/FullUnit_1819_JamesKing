"""test_suite.py: Runs the whole test suite for the indirect reciprocity package"""

__author__ = "James King"

from .action_tests import IdleTests, InteractionTests, GossipTests
from .community_tests import CommunityTest
from .generation_tests import GenerationTest
from .observation_test import ActionObserverTest, PlayerObserverTests
from .player_tests import PlayerStateTests, PlayerTest, PlayerAndStateIntegrationTests
from .facade_tests import FacadeTests

import unittest


def indir_rec_suite():
    suite = unittest.TestSuite()
    suite.addTests([IdleTests(), InteractionTests(), GossipTests(), CommunityTest(), GenerationTest(),
                    ActionObserverTest(), PlayerObserverTests(), PlayerStateTests(), PlayerTest(),
                    PlayerAndStateIntegrationTests(), FacadeTests()])
    return suite


if __name__ == '__main__':
    runner = unittest.TextTestRunner()
    runner.run(indir_rec_suite())
