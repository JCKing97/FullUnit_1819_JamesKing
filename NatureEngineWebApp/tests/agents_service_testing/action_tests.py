import unittest
import requests


def interacting_this_turn(timepoint, player, interactions):
    for interaction in interactions:
        if interaction['timepoint'] == timepoint and interaction['donor'] == player:
            return True
    return False


class ActionTesting(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("PUT", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.standing_payload = {"community": self.community_response['id'], "generation": 0,
                                  "strategy": "Standing Discriminator", "options": ["distrusting"],
                                  "player": 0}
        self.standing_response = requests.request("PUT", self.url + '/agent', json=self.standing_payload).json()
        self.cooperator_payload = {"community": self.community_response['id'], "generation": 0,
                        "strategy": "Cooperator", "options": [],
                        "player": 1}
        self.cooperator_response = requests.request("PUT", self.url + '/agent', json=self.cooperator_payload).json()
        self.defector_payload = {"community": self.community_response['id'], "generation": 0,
                        "strategy": "Defector", "options": [],
                        "player": 2}
        self.defector_response = requests.request("PUT", self.url + '/agent', json=self.defector_payload).json()
        self.percept_defector_negative_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.standing_payload['player'], "about": self.cooperator_payload['player'],
                           "gossiper": self.defector_payload['player'], "timepoint": 2, "gossip": "negative"}
        self.percept_defector_negative_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                 json=self.percept_defector_negative_payload).json()
        self.assertEqual(self.percept_defector_negative_payload, self.percept_defector_negative_response['data'])
        self.assertTrue(self.percept_defector_negative_response['success'])
        self.percept_cooperator_coop_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.standing_payload['player'], "donor": self.cooperator_payload['player'],
                           "recipient": self.defector_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_cooperator_coop_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                 json=self.percept_cooperator_coop_payload).json()
        self.assertEqual(self.percept_cooperator_coop_payload, self.percept_cooperator_coop_response['data'])
        self.assertTrue(self.percept_cooperator_coop_response['success'])
        self.percept_cooperator_defect_payload = {"community": self.community_response['id'], "generation": 0,
                                     "perceiver": self.standing_payload['player'],
                                     "donor": self.cooperator_payload['player'],
                                     "recipient": self.defector_payload['player'], "timepoint": 6, "action": "defect"}
        self.percept_cooperator_defect_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                      json=self.percept_cooperator_defect_payload).json()
        self.assertEqual(self.percept_cooperator_defect_payload, self.percept_cooperator_defect_response['data'])
        self.assertTrue(self.percept_cooperator_defect_response['success'])
        self.percept_cooperator_coop1_payload = {"community": self.community_response['id'], "generation": 0,
                                     "perceiver": self.standing_payload['player'],
                                     "donor": self.cooperator_payload['player'],
                                     "recipient": self.defector_payload['player'], "timepoint": 8, "action": "cooperate"}
        self.percept_cooperator_coop1_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                      json=self.percept_cooperator_coop1_payload).json()
        self.assertEqual(self.percept_cooperator_coop1_payload, self.percept_cooperator_coop1_response['data'])
        self.assertTrue(self.percept_cooperator_coop1_response['success'])
        self.percept_defector_negative1_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.standing_payload['player'],
                                                 "about": self.cooperator_payload['player'],
                                                 "gossiper": self.defector_payload['player'], "timepoint": 10,
                                                 "gossip": "negative"}
        self.percept_defector_negative1_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_defector_negative1_payload).json()
        self.assertEqual(self.percept_defector_negative1_payload, self.percept_defector_negative1_response['data'])
        self.assertTrue(self.percept_defector_negative1_response['success'])
        self.percept_cooperator_negative_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.standing_payload['player'],
                                                 "about": self.defector_payload['player'],
                                                 "gossiper": self.cooperator_payload['player'], "timepoint": 12,
                                                 "gossip": "negative"}
        self.percept_cooperator_negative_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_cooperator_negative_payload).json()
        self.assertEqual(self.percept_cooperator_negative_payload, self.percept_cooperator_negative_response['data'])
        self.assertTrue(self.percept_cooperator_negative_response['success'])
        self.percept_defector_positive_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.standing_payload['player'],
                                                 "about": self.cooperator_payload['player'],
                                                 "gossiper": self.defector_payload['player'], "timepoint": 14,
                                                 "gossip": "positive"}
        self.percept_defector_positive_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_defector_positive_payload).json()
        self.assertEqual(self.percept_defector_positive_payload, self.percept_defector_positive_response['data'])
        self.assertTrue(self.percept_defector_positive_response['success'])
        self.percept_cooperator_negative1_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.standing_payload['player'],
                                                 "about": self.defector_payload['player'],
                                                 "gossiper": self.cooperator_payload['player'], "timepoint": 16,
                                                 "gossip": "negative"}
        self.percept_cooperator_negative1_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_cooperator_negative1_payload).json()
        self.assertEqual(self.percept_cooperator_negative1_payload, self.percept_cooperator_negative1_response['data'])
        self.assertTrue(self.percept_cooperator_negative1_response['success'])
        self.interactions = [{'timepoint': 1, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 1, 'donor': self.defector_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 2, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 2, 'donor': self.standing_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 2, 'donor': self.defector_payload['player'],
                         'recipient': self.standing_payload['player']},
                        {'timepoint': 3, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 4, 'donor': self.standing_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 5, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 5, 'donor': self.cooperator_payload['player'],
                         'recipient': self.standing_payload['player']},
                        {'timepoint': 5, 'donor': self.cooperator_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 6, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 6, 'donor': self.standing_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 7, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 7, 'donor': self.defector_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 10, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 10, 'donor': self.cooperator_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 12, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 12, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 13, 'donor': self.standing_payload['player'],
                         'recipient': self.cooperator_payload['player']},
                        {'timepoint': 16, 'donor': self.standing_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 17, 'donor': self.standing_payload['player'],
                         'recipient': self.defector_payload['player']},
                        {'timepoint': 18, 'donor': self.standing_payload['player'],
                         'recipient': self.defector_payload['player']}]
        for interaction in self.interactions:
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
                                       "generation": 0, "player": self.defector_payload['player']}
            defector_action_response = requests.request("GET", self.url + '/action',
                                                        params=defector_action_payload).json()
            self.assertEqual(defector_action_payload, defector_action_response['data'])
            self.assertTrue(defector_action_response['success'])
            if interacting_this_turn(i, self.defector_payload['player'], self.interactions):
                print("defector: action: " + defector_action_response['action']['type'] + ", timepoint: " + str(i))
                self.assertEqual("defect", defector_action_response['action']['value'])
            else:
                print("defector: action: " + defector_action_response['action']['type'] + ", timepoint: " + str(i))
                self.assertEqual("idle", defector_action_response['action']['type'])

    def test_cooperator_action(self):
        for i in range(20):
            cooperator_action_payload = {"timepoint": i, "community": self.community_response['id'],
                                         "generation": 0, "player": self.cooperator_payload['player']}
            cooperator_action_response = requests.request("GET", self.url + '/action',
                                                        params=cooperator_action_payload).json()
            self.assertEqual(cooperator_action_payload, cooperator_action_response['data'])
            self.assertTrue(cooperator_action_response['success'])
            if interacting_this_turn(i, self.cooperator_payload['player'], self.interactions):
                print("cooperator: action: " + cooperator_action_response['action']['value'] + ", timepoint: " + str(i))
                self.assertEqual("cooperate", cooperator_action_response['action']['value'])
            else:
                print("cooperator: action: " + cooperator_action_response['action']['type'] + ", timepoint: " + str(i))
                self.assertEqual("idle", cooperator_action_response['action']['type'])

    def test_standing_action(self):
        for i in range(20):
            standing_action_payload = {"timepoint": i, "community": self.community_response['id'],
                                         "generation": 0, "player": self.standing_payload['player']}
            standing_action_response = requests.request("GET", self.url + '/action',
                                                        params=standing_action_payload).json()
            self.assertEqual(standing_action_payload, standing_action_response['data'])
            self.assertTrue(standing_action_response['success'])
            if interacting_this_turn(i, self.standing_payload['player'], self.interactions):
                self.assertEqual("action", standing_action_response['action']['type'])
                print("standing: action: " + standing_action_response['action']['type'] + ", timepoint: " + str(i))
                standing_belief_payload = {"timepoint": i, "community": self.community_response['id'], "generation": 0,
                                    "perceiver": self.standing_payload['player'],
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
                print("standing: action: " + standing_action_response['action']['type'] + ", timepoint: " + str(i))
                self.assertEqual("idle", standing_action_response['action']['type'])

    def test_action_incorrect_community(self):
        action_payload = {"timepoint": 8, "community": self.community_response['id']+1000,
                          "generation": 0, "player": self.standing_payload['player']}
        action_response = requests.request("GET", self.url + '/action', params=action_payload).json()
        self.assertFalse(action_response["success"])
        self.assertEqual(action_payload, action_response['data'])
        self.assertEqual("No such community", action_response['message'])

    def test_action_incorrect_generation(self):
        action_payload = {"timepoint": 8, "community": self.community_response['id'],
                          "generation": 99, "player": self.standing_payload['player']}
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


