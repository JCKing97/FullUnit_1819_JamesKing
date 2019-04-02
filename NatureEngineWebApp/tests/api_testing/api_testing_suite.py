"""api_testing_suite.py: Runs the whole test suite for API testing"""

__author__ = "James King"

from tests.api_testing.action_testing import ActionsAPITesting
from tests.api_testing.community_generation_agents_testing import ManagementAPITesting
from tests.api_testing.percepts_testing import PerceptsAPITesting

import unittest

if __name__ == '__main__':
    unittest.main()
