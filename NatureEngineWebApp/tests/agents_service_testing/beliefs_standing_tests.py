import unittest
import requests


class BeliefsStandingTrustingTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("POST", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.perceiver_payload = {"community": self.community_response['id'], "generation": 0,
                                  "strategy": "Standing Discriminator", "options": ["trusting"],
                                  "player": 0}
        self.perceiver_response = requests.request("POST", self.url + '/agent', json=self.perceiver_payload).json()
        self.player1_payload = {"community": self.community_response['id'], "generation": 0,
                                "strategy": "Cooperator", "options": [],
                                "player": 1}
        self.player1_response = requests.request("POST", self.url + '/agent', json=self.player1_payload).json()
        self.player2_payload = {"community": self.community_response['id'], "generation": 0,
                                "strategy": "Defector", "options": [],
                                "player": 2}
        self.player2_response = requests.request("POST", self.url + '/agent', json=self.player2_payload).json()
        self.percept_player2_negative_payload = {"community": self.community_response['id'], "generation": 0,
                                                 "perceiver": self.perceiver_payload['player'],
                                                 "about": self.player1_payload['player'],
                                                 "gossiper": self.player2_payload['player'], "timepoint": 2,
                                                 "gossip": "negative"}
        self.percept_player2_negative_response = requests.request("POST", self.url + '/percept/action/gossip',
                                                                  json=self.percept_player2_negative_payload).json()
        self.assertEqual(self.percept_player2_negative_payload, self.percept_player2_negative_response['data'])
        self.assertTrue(self.percept_player2_negative_response['success'])
        self.percept_player1_coop_payload = {"community": self.community_response['id'], "generation": 0,
                                             "perceiver": self.perceiver_payload['player'],
                                             "donor": self.player1_payload['player'],
                                             "recipient": self.player2_payload['player'], "timepoint": 4,
                                             "action": "cooperate"}
        self.percept_player1_coop_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                              json=self.percept_player1_coop_payload).json()
        self.assertEqual(self.percept_player1_coop_payload, self.percept_player1_coop_response['data'])
        self.assertTrue(self.percept_player1_coop_response['success'])
        self.percept_player1_defect_payload = {"community": self.community_response['id'], "generation": 0,
                                               "perceiver": self.perceiver_payload['player'],
                                               "donor": self.player1_payload['player'],
                                               "recipient": self.player2_payload['player'], "timepoint": 6,
                                               "action": "defect"}
        self.percept_player1_defect_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                                json=self.percept_player1_defect_payload).json()
        self.assertEqual(self.percept_player1_defect_payload, self.percept_player1_defect_response['data'])
        self.assertTrue(self.percept_player1_defect_response['success'])
        self.percept_player1_coop1_payload = {"community": self.community_response['id'], "generation": 0,
                                              "perceiver": self.perceiver_payload['player'],
                                              "donor": self.player1_payload['player'],
                                              "recipient": self.player2_payload['player'], "timepoint": 8,
                                              "action": "defect"}
        self.percept_player1_coop1_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                               json=self.percept_player1_coop1_payload).json()
        self.assertEqual(self.percept_player1_coop1_payload, self.percept_player1_coop1_response['data'])
        self.assertTrue(self.percept_player1_coop1_response['success'])
        self.percept_player1_coop1_payload = {"community": self.community_response['id'], "generation": 0,
                                              "perceiver": self.perceiver_payload['player'],
                                              "donor": self.player1_payload['player'],
                                              "recipient": self.player2_payload['player'], "timepoint": 9,
                                              "action": "cooperate"}
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

    def test_initial_standing(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertTrue(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("good", initial_belief_player1_response['standing'])
        initial_belief_player2_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player2_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player2_payload).json()
        self.assertTrue(initial_belief_player2_response['success'])
        self.assertEqual(initial_belief_player2_payload, initial_belief_player2_response['data'])
        self.assertEqual("good", initial_belief_player2_response['standing'])

    def test_incorrect_community(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id']+11000, "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such community", initial_belief_player1_response['message'])

    def test_incorrect_generation(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 3,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such generation for this community", initial_belief_player1_response['message'])

    def test_incorrect_perceiver(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": 300,
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such agent as perceiver for this generation of this community", initial_belief_player1_response['message'])

    def test_incorrect_about(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": 400}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such agent as about for this generation of this community", initial_belief_player1_response['message'])

    def test_incorrect_perceiver_and_about(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": 300,
                                          "about": 400}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such agent as perceiver or about for this generation of this community",
                         initial_belief_player1_response['message'])

    def test_perceiver_not_using_standing_strategy(self):
        standing_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.player1_payload['player'],
                                          "about": self.perceiver_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=standing_payload).json()
        self.assertFalse(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("The perceiver is not using the standing strategy so has no beliefs on other players standings",
                         standing_response['message'])

    def test_after_negative_gossip_from_trusted_agent_timepoint3(self):
        standing_payload = {"timepoint": 3, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_after_negative_gossip_from_trusted_agent_timepoint11(self):
        standing_payload = {"timepoint": 11, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_after_negative_gossip_from_trusted_agent_timepoint17(self):
        standing_payload = {"timepoint": 17, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player2_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_after_negative_gossip_from_untrusted_agent_timepoint13(self):
        standing_payload = {"timepoint": 3, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player2_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_restorative_coop_timepoint5(self):
        standing_payload = {"timepoint": 5, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_restorative_coop_timepoint9(self):
        standing_payload = {"timepoint": 9, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_destructive_defect_timepoint7(self):
        standing_payload = {"timepoint": 7, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_after_another_defect_timepoint8(self):
        standing_payload = {"timepoint": 8, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_afer_positive_restorative_gossip_timepoint15(self):
        standing_payload = {"timepoint": 15, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])


class BeliefsStandingDistrustingTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("POST", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.perceiver_payload = {"community": self.community_response['id'], "generation": 0,
                                  "strategy": "Standing Discriminator", "options": ["distrusting"],
                                  "player": 0}
        self.perceiver_response = requests.request("POST", self.url + '/agent', json=self.perceiver_payload).json()
        self.player1_payload = {"community": self.community_response['id'], "generation": 0,
                        "strategy": "Cooperator", "options": [],
                        "player": 1}
        self.player1_response = requests.request("POST", self.url + '/agent', json=self.player1_payload).json()
        self.player2_payload = {"community": self.community_response['id'], "generation": 0,
                        "strategy": "Defector", "options": [],
                        "player": 2}
        self.player2_response = requests.request("POST", self.url + '/agent', json=self.player2_payload).json()
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
                                     "recipient": self.player2_payload['player'], "timepoint": 8, "action": "defect"}
        self.percept_player1_coop1_response = requests.request("POST", self.url + '/percept/action/interaction',
                                                      json=self.percept_player1_coop1_payload).json()
        self.assertEqual(self.percept_player1_coop1_payload, self.percept_player1_coop1_response['data'])
        self.assertTrue(self.percept_player1_coop1_response['success'])
        self.percept_player1_coop1_payload = {"community": self.community_response['id'], "generation": 0,
                                              "perceiver": self.perceiver_payload['player'],
                                              "donor": self.player1_payload['player'],
                                              "recipient": self.player2_payload['player'], "timepoint": 9,
                                              "action": "cooperate"}
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
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "donor": self.perceiver_payload['player'], "recipient": self.player1_payload['player'],
                           "timepoint": 1}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])

    def test_initial_standing(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertTrue(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("good", initial_belief_player1_response['standing'])
        initial_belief_player2_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player2_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player2_payload).json()
        self.assertTrue(initial_belief_player2_response['success'])
        self.assertEqual(initial_belief_player2_payload, initial_belief_player2_response['data'])
        self.assertEqual("good", initial_belief_player2_response['standing'])

    def test_incorrect_community(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id']+11000, "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such community", initial_belief_player1_response['message'])

    def test_incorrect_generation(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 3,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such generation for this community", initial_belief_player1_response['message'])

    def test_incorrect_perceiver(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": 300,
                                          "about": self.player1_payload['player']}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such agent as perceiver for this generation of this community", initial_belief_player1_response['message'])

    def test_incorrect_about(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.perceiver_payload['player'],
                                          "about": 400}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such agent as about for this generation of this community", initial_belief_player1_response['message'])

    def test_incorrect_perceiver_and_about(self):
        initial_belief_player1_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": 300,
                                          "about": 400}
        initial_belief_player1_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=initial_belief_player1_payload).json()
        self.assertFalse(initial_belief_player1_response['success'])
        self.assertEqual(initial_belief_player1_payload, initial_belief_player1_response['data'])
        self.assertEqual("No such agent as perceiver or about for this generation of this community",
                         initial_belief_player1_response['message'])

    def test_perceiver_not_using_standing_strategy(self):
        standing_payload = {"timepoint": 1, "community": self.community_response['id'], "generation": 0,
                                          "perceiver": self.player1_payload['player'],
                                          "about": self.perceiver_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                                           params=standing_payload).json()
        self.assertFalse(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("The perceiver is not using the standing strategy so has no beliefs on other players standings",
                         standing_response['message'])

    def test_after_negative_gossip_from_trusted_agent_timepoint3(self):
        standing_payload = {"timepoint": 3, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_negative_gossip_from_trusted_agent_timepoint11(self):
        standing_payload = {"timepoint": 11, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_negative_gossip_from_trusted_agent_timepoint17(self):
        standing_payload = {"timepoint": 17, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player2_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_negative_gossip_from_untrusted_agent_timepoint13(self):
        standing_payload = {"timepoint": 3, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player2_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_restorative_coop_timepoint5(self):
        standing_payload = {"timepoint": 5, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_restorative_coop_timepoint9(self):
        standing_payload = {"timepoint": 9, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

    def test_after_destructive_defect_timepoint7(self):
        standing_payload = {"timepoint": 7, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_after_another_defect_timepoint8(self):
        standing_payload = {"timepoint": 8, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("bad", standing_response['standing'])

    def test_afer_positive_restorative_gossip_timepoint15(self):
        standing_payload = {"timepoint": 15, "community": self.community_response['id'], "generation": 0,
                            "perceiver": self.perceiver_payload['player'],
                            "about": self.player1_payload['player']}
        standing_response = requests.request("GET", self.url + '/belief/standing',
                                             params=standing_payload).json()
        self.assertTrue(standing_response['success'])
        self.assertEqual(standing_payload, standing_response['data'])
        self.assertEqual("good", standing_response['standing'])

