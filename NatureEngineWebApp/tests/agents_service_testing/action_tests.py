import unittest
import requests


class ActionTesting(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("PUT", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.perceiver_payload = {"community": self.community_response['id'], "generation": 0,
                                  "strategy": "Standing Discriminator", "options": ["distrusting"],
                                  "player": 0}
        self.perceiver_response = requests.request("PUT", self.url + '/agent', json=self.perceiver_payload).json()
        self.player1_payload = {"community": self.community_response['id'], "generation": 0,
                        "strategy": "Cooperator", "options": [],
                        "player": 1}
        self.player1_response = requests.request("PUT", self.url + '/agent', json=self.player1_payload).json()
        self.player2_payload = {"community": self.community_response['id'], "generation": 0,
                        "strategy": "Defector", "options": [],
                        "player": 2}
        self.player2_response = requests.request("PUT", self.url + '/agent', json=self.player2_payload).json()
        self.percept_player2_negative_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": self.player1_payload['player'],
                           "gossiper": self.player2_payload['player'], "timepoint": 2, "gossip": "negative"}
        self.percept_player2_negative_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                 json=self.percept_player2_negative_payload).json()
        self.assertEqual(self.percept_player2_negative_payload, self.percept_player2_negative_response['data'])
        self.assertTrue(self.percept_player2_negative_response['success'])
        self.percept_player1_coop_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": self.player1_payload['player'],
                           "recipient": self.player2_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_player1_coop_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                 json=self.percept_player1_coop_payload).json()
        self.assertEqual(self.percept_player1_coop_payload, self.percept_player1_coop_response['data'])
        self.assertTrue(self.percept_player1_coop_response['success'])
        self.percept_player1_defect_payload = {"community": self.community_response['id'], "generation": 0,
                                     "perceiver": self.perceiver_payload['player'],
                                     "donor": self.player1_payload['player'],
                                     "recipient": self.player2_payload['player'], "timepoint": 6, "action": "defect"}
        self.percept_player1_defect_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                      json=self.percept_player1_defect_payload).json()
        self.assertEqual(self.percept_player1_defect_payload, self.percept_player1_defect_response['data'])
        self.assertTrue(self.percept_player1_defect_response['success'])
        self.percept_player1_coop1_payload = {"community": self.community_response['id'], "generation": 0,
                                     "perceiver": self.perceiver_payload['player'],
                                     "donor": self.player1_payload['player'],
                                     "recipient": self.player2_payload['player'], "timepoint": 8, "action": "cooperate"}
        self.percept_player1_coop1_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                      json=self.percept_player1_coop1_payload).json()
        self.assertEqual(self.percept_player1_coop1_payload, self.percept_player1_coop1_response['data'])
        self.assertTrue(self.percept_player1_coop1_response['success'])
        self.percept_player2_negative1_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.perceiver_payload['player'],
                                                 "about": self.player1_payload['player'],
                                                 "gossiper": self.player2_payload['player'], "timepoint": 10,
                                                 "gossip": "negative"}
        self.percept_player2_negative1_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_player2_negative1_payload).json()
        self.assertEqual(self.percept_player2_negative1_payload, self.percept_player2_negative1_response['data'])
        self.assertTrue(self.percept_player2_negative1_response['success'])
        self.percept_player1_negative_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.perceiver_payload['player'],
                                                 "about": self.player2_payload['player'],
                                                 "gossiper": self.player1_payload['player'], "timepoint": 12,
                                                 "gossip": "negative"}
        self.percept_player1_negative_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_player1_negative_payload).json()
        self.assertEqual(self.percept_player1_negative_payload, self.percept_player1_negative_response['data'])
        self.assertTrue(self.percept_player1_negative_response['success'])
        self.percept_player2_positive_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.perceiver_payload['player'],
                                                 "about": self.player1_payload['player'],
                                                 "gossiper": self.player2_payload['player'], "timepoint": 14,
                                                 "gossip": "positive"}
        self.percept_player2_positive_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_player2_positive_payload).json()
        self.assertEqual(self.percept_player2_positive_payload, self.percept_player2_positive_response['data'])
        self.assertTrue(self.percept_player2_positive_response['success'])
        self.percept_player1_negative1_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.perceiver_payload['player'],
                                                 "about": self.player2_payload['player'],
                                                 "gossiper": self.player1_payload['player'], "timepoint": 16,
                                                 "gossip": "negative"}
        self.percept_player1_negative1_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_player1_negative1_payload).json()
        self.assertEqual(self.percept_player1_negative1_payload, self.percept_player1_negative1_response['data'])
        self.assertTrue(self.percept_player1_negative1_response['success'])
        interactions = [{'timepoint': 1, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 1, 'donor': self.player2_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 2, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 2, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 2, 'donor': self.player2_payload['player'],
                         'recipient': self.perceiver_payload['player']},
                        {'timepoint': 3, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 4, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 5, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 5, 'donor': self.player1_payload['player'],
                         'recipient': self.perceiver_payload['player']},
                        {'timepoint': 5, 'donor': self.player1_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 6, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 6, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 7, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 7, 'donor': self.player2_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 10, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 10, 'donor': self.player1_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 12, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 12, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 13, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player1_payload['player']},
                        {'timepoint': 16, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 17, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player2_payload['player']},
                        {'timepoint': 18, 'donor': self.perceiver_payload['player'],
                         'recipient': self.player2_payload['player']}]
        for interaction in interactions:
            percept_payload = {"community": self.community_response['id'], "generation": 0,
                               "donor": interaction['donor'], "recipient": interaction['recipient'],
                               "timepoint": interaction['timepoint']}
            self.percept_response = requests.request("POST", self.url + '/percept/interaction',
                                                     json=percept_payload).json()
            self.assertEqual(percept_payload, self.percept_response['data'])
            self.assertTrue(self.percept_response['success'])

    def test_defector_action(self):
        for i in range(20):
            defector_action_payload = {"timepoint": i, "community": self.community_response['id'],
                                       "generation": 0, "player": self.player2_payload['player']}
            defector_action_response = requests.request("GET", self.url + '/action',
                                                        params=defector_action_payload).json()
            self.assertEqual(defector_action_payload, defector_action_response['data'])
            self.assertTrue(defector_action_response['success'])
            belief_params = {"timepoint": i, "community": self.community_response['id'],
                             "generation": 0, "player": self.player2_payload['player']}
            belief_response = requests.request("GET", self.url + '/belief/donor',
                                                     params=belief_params).json()
            if belief_response['timepoint'] != -1 and belief_response['timepoint'] == i:
                self.assertEqual("defect", defector_action_response['action']['value'])
            else:
                self.assertEqual("idle", defector_action_response['action']['type'])

    def test_cooperator_action(self):
        for i in range(20):
            cooperator_action_payload = {"timepoint": i, "community": self.community_response['id'],
                                         "generation": 0, "player": self.player1_payload['player']}
            cooperator_action_response = requests.request("GET", self.url + '/action',
                                                        params=cooperator_action_payload).json()
            self.assertEqual(cooperator_action_payload, cooperator_action_response['data'])
            self.assertTrue(cooperator_action_response['success'])
            belief_params = {"timepoint": i, "community": self.community_response['id'],
                             "generation": 0, "player": self.player1_payload['player']}
            belief_response = requests.request("GET", self.url + '/belief/donor',
                                                     params=belief_params).json()
            if belief_response['timepoint'] != -1 and  belief_response['timepoint'] == i:
                self.assertEqual("defect", cooperator_action_response['action']['value'])
            else:
                self.assertEqual("idle", cooperator_action_response['action']['type'])

    def test_standing_action(self):
        for i in range(20):
            standing_action_payload = {"timepoint": i, "community": self.community_response['id'],
                                         "generation": 0, "player": self.perceiver_payload['player']}
            standing_action_response = requests.request("GET", self.url + '/action',
                                                        params=standing_action_payload).json()
            self.assertEqual(standing_action_payload, standing_action_response['data'])
            self.assertTrue(standing_action_response['success'])
            donor_belief_params = {"timepoint": i, "community": self.community_response['id'],
                                   "generation": 0, "player": self.player1_payload['player']}
            donor_belief_response = requests.request("GET", self.url + '/belief/donor',
                                               params=donor_belief_params).json()
            if donor_belief_response['timepoint'] != -1 and  donor_belief_response['timepoint'] == i:
                standing_belief_payload = {"timepoint": i, "community": self.community_response['id'], "generation": 0,
                                    "perceiver": self.perceiver_payload['player'],
                                    "about": standing_action_response['action']['recipient']}
                standing_belief_response = requests.request("GET", self.url + '/belief/standing',
                                                     params=standing_belief_payload).json()
                if standing_belief_response['standing'] == "good":
                    self.assertEqual("cooperate", standing_action_response['action']['value'])
                elif standing_belief_response['standing'] == "bad":
                    self.assertEqual("defect", standing_action_response['action']['value'])
                else:
                    self.fail(msg="Incorrect standing")
            else:
                self.assertEqual("idle", standing_action_response['action']['type'])

    def test_action_incorrect_community(self):
        action_payload = {"timepoint": 8, "community": self.community_response['id']+1000,
                          "generation": 0, "player": self.perceiver_payload['player']}
        action_response = requests.request("GET", self.url + '/action', params=action_payload).json()
        self.assertFalse(action_response["success"])
        self.assertEqual(action_payload, action_response['data'])
        self.assertEqual("No such community", action_response['message'])

    def test_action_incorrect_generation(self):
        action_payload = {"timepoint": 8, "community": self.community_response['id'],
                          "generation": 99, "player": self.perceiver_payload['player']}
        action_response = requests.request("GET", self.url + '/action', params=action_payload).json()
        self.assertFalse(action_response["success"])
        self.assertEqual(action_payload, action_response['data'])
        self.assertEqual("No such generation for this community", action_response['message'])

    def test_action_incorrect_player(self):
        action_payload = {"timepoint": 8, "community": self.community_response['id'],
                          "generation": 0, "player": 300}
        action_response = requests.request("GET", self.url + '/action', params=action_payload).json()
        self.assertFalse(action_response["success"])
        self.assertEqual(action_payload, action_response['data'])
        self.assertEqual("No such player for this generation of this community", action_response['message'])


