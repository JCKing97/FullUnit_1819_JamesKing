"""percepts_testing.py: Testing the api for sending percepts to agents"""

__author__ = "James King"

import unittest
import requests

api_url = "http://127.0.0.1:8080/"


class ActionsAPITesting(unittest.TestCase):

    community_id = 0
    gen = 0
    agent = 0
    agent1 = 1

    def setUp(self):
        self.community_id = requests.post(api_url + "community").json()['id']
        requests.post(api_url + "generation", json={"community": self.community_id, "generation": self.gen})
        body = {"community": self.community_id, "generation": self.gen, "donor_strategy": "Veritability Discerner",
                "non_donor_strategy": "Promote Self", "trust_model": "Strong Reactor", "options": [5],
                "player": self.agent}
        requests.post(api_url + "agent", json=body)
        body["player"] = self.agent1
        requests.post(api_url + "agent", json=body)

    def test_get_action(self):
        action_body = {"community": self.community_id, "generation": self.gen, "player": self.agent, "timepoint": 3}
        action_response = requests.get(api_url+"action?timepoint="+str(action_body["timepoint"])+"&community="+
                                       str(action_body["community"])+"&generation="+str(action_body["generation"])+
                                       "&player="+str(action_body["player"]))
        self.assertEqual(action_response.status_code, 200)
        self.assertEqual(action_response.json(), {"data": action_body, "success": True, "status": 200,
                                                  "action": {"type": "gossip", "value": "positive", "about": self.agent,
                                                             "recipient": self.agent1,
                                                             "reason": "I promote myself because I want"
                                                                       " to encourage others to cooperate with me"}})

    def test_get_action_incorrect(self):
        action_body = {"community": self.community_id, "generation": -20, "player": self.agent, "timepoint": 3}
        action_response = requests.get(api_url + "action?timepoint=" + str(action_body["timepoint"]) + "&community=" +
                                       str(action_body["community"]) + "&generation=" + str(action_body["generation"]) +
                                       "&player=" + str(action_body["player"]))
        self.assertEqual(action_response.status_code, 404)
        self.assertEqual(action_response.json(), {"data": action_body, "success": False, "status": 404,
                                                  "message": "No such generation for this community"})
