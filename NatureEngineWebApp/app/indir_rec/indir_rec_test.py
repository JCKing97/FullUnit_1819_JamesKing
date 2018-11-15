"""indir_rec_test.py: Test the indirect reciprocity model implementation"""

__author__ = "James King adapted from Miguel Grinberg"

import unittest
from tests.test_config import TestConfig
from app import create_app
from .community_logic import Community, CommunityCreationException
from .generation_logic import Generation, GenerationCreationException


class CreationTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()

    def tearDown(self):
        self.app_context.pop()

    def test_create_community(self):
        strategies = {"cooperator": 2, "defector": 2}
        try:
            Community(initial_strategies=strategies)
        except CommunityCreationException:
            self.fail("Shouldn't fail to create the community")

    def test_create_generation_and_players(self):
        strategies = {"cooperator": 2, "defector": 2}
        try:
            community = Community(initial_strategies=strategies)
        except CommunityCreationException:
            self.fail("Shouldn't fail to create the community")
        try:
            Generation(strategies, communityID=community.get_id(),
                       start_time=0, end_time=10, id=0, onlooker_number=5)
        except GenerationCreationException:
            self.fail("Shouldn't fail to create the generation")




