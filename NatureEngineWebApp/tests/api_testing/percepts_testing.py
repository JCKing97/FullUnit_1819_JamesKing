"""percepts_testing.py: Testing the api for sending percepts to agents"""

__author__ = "James King"

import unittest
import requests

api_url = "http://127.0.0.1:8080/"


class PerceptsAPITesting(unittest.TestCase):

    community_id = 0
    gen = 0
    agent1 = 0
    agent2 = 1
    agent3 = 2

    def setUp(self):
        self.community_id = requests.post(api_url + "community").json()['id']
        requests.post(api_url + "generation", json={"community": self.community_id, "generation": self.gen})
        body = {"community": self.community_id, "generation": self.gen, "donor_strategy": "Veritability Discerner",
                "non_donor_strategy": "Promote Self", "trust_model": "Strong Reactor", "options": [5],
                "player": self.agent1}
        requests.post(api_url + "agent", json=body)
        body["player"] = self.agent2
        requests.post(api_url + "agent", json=body)
        body["player"] = self.agent3
        requests.post(api_url + "agent", json=body)

    def test_percept_action_interaction(self):
        percept_body = {"community": self.community_id, "generation": self.gen, "perceiver": self.agent1,
                        "action": "defect", "donor": self.agent2, "recipient": self.agent3, "timepoint": 3}
        percept_response = requests.post(api_url+"percept/action/interaction", json=percept_body)
        self.assertEqual(percept_response.status_code, 200)
        self.assertEqual(percept_response.json(), {"data": percept_body, "success": True, "status": 200})

    def test_percept_action_interaction_incorrect_body(self):
        percept_body = {"community": self.community_id, "generation": self.gen, "perceiver": self.agent1,
                        "action": "incorrect", "donor": self.agent2, "recipient": self.agent3, "timepoint": 3}
        percept_response = requests.post(api_url + "percept/action/interaction", json=percept_body)
        self.assertEqual(percept_response.status_code, 404)
        self.assertEqual(percept_response.json(), {"data": percept_body, "success": False, "status": 404,
                                                   "message": "Incorrect action must either be defect or cooperate"})

    def test_percept_action_gossip(self):
        percept_body = {"community": self.community_id, "generation": self.gen, "perceiver": self.agent1,
                        "gossip": "positive", "gossiper": self.agent2, "about": self.agent3, "timepoint": 3}
        percept_response = requests.post(api_url + "percept/action/gossip", json=percept_body)
        self.assertEqual(percept_response.status_code, 200)
        self.assertEqual(percept_response.json(), {"data": percept_body, "success": True, "status": 200})

    def test_percept_action_gossip_incorrect_body(self):
        percept_body = {"community": self.community_id, "generation": self.gen, "perceiver": self.agent1,
                        "gossip": "incorrect", "gossiper": self.agent2, "about": self.agent3, "timepoint": 3}
        percept_response = requests.post(api_url + "percept/action/gossip", json=percept_body)
        self.assertEqual(percept_response.status_code, 404)
        self.assertEqual(percept_response.json(), {"data": percept_body, "success": False, "status": 404,
                                                   "message": "Incorrect gossip action"
                                                              " should be either positive or negative"})

    def test_percept_interaction(self):
        percept_body = {"community": self.community_id, "generation": self.gen, "donor": self.agent2,
                        "recipient": self.agent3, "timepoint": 3}
        percept_response = requests.post(api_url + "percept/interaction", json=percept_body)
        self.assertEqual(percept_response.status_code, 200)
        self.assertEqual(percept_response.json(), {"data": percept_body, "success": True, "status": 200})

    def test_percept_interaction_incorrect(self):
        percept_body = {"community": self.community_id, "generation": -20, "donor": self.agent2,
                        "recipient": self.agent3, "timepoint": 3}
        percept_response = requests.post(api_url + "percept/interaction", json=percept_body)
        self.assertEqual(percept_response.status_code, 404)
        self.assertEqual(percept_response.json(), {"data": percept_body, "success": False, "status": 404,
                                                   "message": "No such generation for this community"})
