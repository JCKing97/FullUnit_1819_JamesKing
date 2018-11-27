import requests
import unittest


class PerceptInteractionTests(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("PUT", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.donor_payload = {"strategy": "Defector", "options": [], "community": self.community_response['id'],
                         "generation": 0, "player": 0}
        self.donor_response = requests.request("PUT", self.url + '/agent', json=self.donor_payload).json()
        self.assertEqual(self.donor_response['data'], self.donor_payload)
        self.assertTrue(self.donor_response['success'])
        self.recipient_payload = {"strategy": "Cooperator", "options": [], "community": self.community_response['id'],
                                  "generation": 0, "player": 1}
        self.recipient_response = requests.request("PUT", self.url + '/agent', json=self.recipient_payload).json()
        self.assertEqual(self.recipient_response['data'], self.recipient_payload)
        self.assertTrue(self.recipient_response['success'])

    def test_interaction_percept(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "donor": self.donor_payload['player'], "recipient": self.recipient_payload['player'],
                           "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])

    def test_interaction_percept_incorrect_community(self):
        percept_payload = {"community": self.community_response['id']+10000, "generation": 0, "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such community')

    def test_interaction_percept_incorrect_generation(self):
        percept_payload = {"community": self.community_response['id'], "generation": 7, "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such generation for this community')

    def test_interaction_percept_incorrect_donor(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0, "donor": 9,
                           "recipient": self.recipient_payload['player'], "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such donor for this community and generation')

    def test_interaction_percept_incorrect_recipient(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0, "donor": self.donor_payload['player'],
                           "recipient": 177, "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such recipient for this community and generation')

    def test_interaction_percept_incorrect_recipient_and_donor(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0, "donor": 17,
                           "recipient": 177, "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such recipient or donor for this community and generation')


class PerceptActionInteractionTests(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("PUT", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.donor_payload = {"strategy": "Defector", "options": [], "community": self.community_response['id'],
                              "generation": 0, "player": 0}
        self.donor_response = requests.request("PUT", self.url + '/agent', json=self.donor_payload).json()
        self.assertEqual(self.donor_response['data'], self.donor_payload)
        self.assertTrue(self.donor_response['success'])
        self.recipient_payload = {"strategy": "Cooperator", "options": [], "community": self.community_response['id'],
                                  "generation": 0, "player": 1}
        self.recipient_response = requests.request("PUT", self.url + '/agent', json=self.recipient_payload).json()
        self.assertEqual(self.recipient_response['data'], self.recipient_payload)
        self.assertTrue(self.recipient_response['success'])
        self.perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": self.community_response['id'], "generation": 0, "player": 2}
        self.perceiver_response = requests.request("PUT", self.url + '/agent', json=self.perceiver_payload).json()
        self.assertEqual(self.perceiver_response['data'], self.perceiver_payload)
        self.assertTrue(self.perceiver_response['success'])

    def test_action_interaction_incorrect_community(self):
        percept_payload = {"community": self.community_response['id']+10000, "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such community')

    def test_action_interaction_percept_incorrect_generation(self):
        percept_payload = {"community": self.community_response['id'], "generation": 8,
                           "perceiver": self.perceiver_payload['player'], "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such generation for this community')

    def test_action_interaction_percept_incorrect_perceiver(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 973, "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver')

    def test_action_interaction_percept_incorrect_donor(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": 777,
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: donor')

    def test_action_interaction_percept_incorrect_recipient(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": self.donor_payload['player'],
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: recipient')

    def test_action_interaction_percept_incorrect_perceiver_and_donor_and_recipient(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 888, "donor": 777,
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver, donor or recipient')

    def test_action_interaction_percept_incorrect_perceiver_and_donor(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 888, "donor": 777,
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver or donor')

    def test_action_interaction_percept_incorrect_donor_and_recipient(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": 777,
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: donor or recipient')

    def test_action_interaction_percept_incorrect_perceiver_and_recipient(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 888, "donor": self.donor_payload['player'],
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver or recipient')

    def test_action_interaction_percept_incorrect_action(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "failure"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'Incorrect action must either be defect or cooperate')

    def test_action_interaction_percept_cooperate(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])

    def test_action_interaction_percept_defect(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4, "action": "defect"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])


class PerceptActionGossipTests(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("PUT", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                                  "community": self.community_response['id'], "generation": 0, "player": 2}
        self.perceiver_response = requests.request("PUT", self.url + '/agent', json=self.perceiver_payload).json()
        self.assertEqual(self.perceiver_response['data'], self.perceiver_payload)
        self.assertTrue(self.perceiver_response['success'])
        self.about_payload = {"strategy": "Defector", "options": [], "community": self.community_response['id'],
                         "generation": 0, "player": 0}
        self.about_response = requests.request("PUT", self.url + '/agent', json=self.about_payload).json()
        self.assertEqual(self.about_payload, self.about_response['data'])
        self.assertTrue(self.about_response['success'])
        self.gossiper_payload = {"strategy": "Cooperator", "options": [], "community": self.community_response['id'],
                            "generation": 0, "player": 1}
        self.gossiper_response = requests.request("PUT", self.url + '/agent', json=self.gossiper_payload).json()
        self.assertEqual(self.gossiper_payload, self.gossiper_response['data'])
        self.assertTrue(self.gossiper_response['success'])

    def test_action_gossip_percept_incorrect_community(self):
        percept_payload = {"community": self.community_response['id']+92102, "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": self.about_payload['player'],
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such community')

    def test_action_gossip_percept_incorrect_generation(self):
        percept_payload = {"community": self.community_response['id'], "generation": 700,
                           "perceiver": self.perceiver_payload['player'], "about": self.about_payload['player'],
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such generation for this community')

    def test_action_gossip_percept_incorrect_perceiver(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 600, "about": self.about_payload['player'],
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver')

    def test_action_gossip_percept_incorrect_about(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": 11,
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: about')

    def test_action_gossip_percept_incorrect_gossiper(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": self.about_payload['player'],
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: gossiper')

    def test_action_gossip_percept_incorrect_perceiver_and_about_and_gossiper(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 6786, "about": 7000,
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver, gossiper or about')

    def test_action_gossip_percept_incorrect_perceiver_and_about(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 600, "about": 78,
                           "gossiper":  self.gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver or about')

    def test_action_gossip_percept_incorrect_perceiver_and_gossiper(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": 800, "about": self.about_payload['player'],
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: perceiver or gossiper')

    def test_action_gossip_percept_incorrect_gossiper_and_about(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": 9287,
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'No such agent for: gossiper or about')

    def test_action_gossip_percept_incorrect_gossip(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": self.about_payload['player'],
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "failure"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertFalse(self.percept_response['success'])
        self.assertEqual(self.percept_response['message'], 'Incorrect gossip action should be either positive or negative')

    def test_action_gossip_percept_positive(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": self.about_payload['player'],
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "positive"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])

    def test_action_gossip_percept_negative(self):
        percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "perceiver": self.perceiver_payload['player'], "about": self.about_payload['player'],
                           "gossiper": self.gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        self.percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])