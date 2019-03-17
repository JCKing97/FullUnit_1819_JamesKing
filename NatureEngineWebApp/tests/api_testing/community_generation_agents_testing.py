"""community_generation_agents_testing.py: Testing the API for creation and destruction of communities"""

__author__ = "James King"

import unittest
import requests
import urllib

api_url = "http://127.0.0.1:8080/"


class ManagementAPITesting(unittest.TestCase):

    community_id = 0

    def setUp(self):
        id = requests.post(api_url + "community").json()['id']+1
        create_community_response = requests.post(api_url+"community")
        self.assertEqual(create_community_response.status_code, 200)
        self.assertEqual(create_community_response.json(), {"success": True, "status": 200, "id": id})
        self.community_id = id

    def test_delete_existing_community(self):
        delete_community_response = requests.delete(api_url+"community?community="+str(self.community_id))
        self.assertEqual(delete_community_response.status_code, 200)
        self.assertEqual(delete_community_response.json(), {"success": True, "status": 200,
                                                            "data": {"community": self.community_id}})

    def test_delete_non_existing_community(self):
        delete_community_response = requests.delete(api_url + "community?community="+str(self.community_id+1))
        self.assertEqual(delete_community_response.status_code, 404)
        self.assertEqual(delete_community_response.json(), {"success": False, "status": 404,
                                                            "message": "No community with this ID to retract",
                                                            "data": {"community": self.community_id+1}})

    def test_post_new_generation_and_duplicate(self):
        body = {"community": self.community_id, "generation": 0}
        # Create a new generation
        create_gen_response = requests.post(api_url+"generation", json=body)
        self.assertEqual(create_gen_response.status_code, 200)
        self.assertEqual(create_gen_response.json(), {"data": body, "success": True, "status": 200})
        # Attempt to create a duplicate
        create_duplicate_response = requests.post(api_url + "generation", json=body)
        self.assertEqual(create_duplicate_response.status_code, 404)
        self.assertEqual(create_duplicate_response.json(), {"data": body,
                                                            "message": "This community already"
                                                                       " has a generation with this id",
                                                            "success": False, "status": 404})

    def test_post_new_agent_and_duplicate(self):
        gen = 0
        requests.post(api_url + "generation", json={"community": self.community_id, "generation": gen})
        # Create a new player
        body = {"community": self.community_id, "generation": gen, "player": 9,
                "donor_strategy": "Veritability Discerner", "non_donor_strategy": "Promote Self",
                "trust_model": "Strong Reactor",  "options": [5]}
        create_player_response = requests.post(api_url + "agent", json=body)
        self.assertEqual(create_player_response.status_code, 200)
        self.assertEqual(create_player_response.json(), {"data": body, "success": True, "status": 200})
        # Attempt to create a duplicate
        create_duplicate_response = requests.post(api_url + "agent", json=body)
        self.assertEqual(create_duplicate_response.status_code, 404)
        self.assertEqual(create_duplicate_response.json(), {"data": body, "success": False, "status": 404,
                                                            "message": "Player ID already taken"
                                                                       " for this community and generation"})

    def test_post_incorrect_strategy(self):
        gen = 0
        requests.post(api_url + "generation", json={"community": self.community_id, "generation": gen})
        # Create a new player
        body = {"community": self.community_id, "generation": gen, "player": 9,
                "donor_strategy": "V Discerner", "non_donor_strategy": "Promote Self",
                "trust_model": "Strong Reactor", "options": [5]}
        create_player_response = requests.post(api_url + "agent", json=body)
        self.assertEqual(create_player_response.status_code, 404)
        self.assertEqual(create_player_response.json(), {"data": body, "success": False, "status": 404,
                                                         "message": "No such strategy"})



