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
        percept_payload = {"community": self.community_response['id'], "generation": 0, "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4}
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


class BeliefsDonorRecipientInteractionTesting(unittest.TestCase):

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
        self.percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=self.percept_payload).json()
        self.assertEqual(self.percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])

    def test_donor_belief(self):
        donor_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                               "generation": 0, "player": self.donor_payload['player']}
        donor_belief_response = requests.request("GET", self.url + '/belief/donor', params=donor_belief_params).json()
        self.assertEqual(donor_belief_params, donor_belief_response['data'])
        self.assertTrue(donor_belief_response['success'])
        self.assertEqual(4, donor_belief_response['timepoint'])
        self.assertEqual(self.recipient_payload['player'], donor_belief_response['recipient'])

    def test_recipient_belief(self):
        recipient_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                   "generation": 0, "player": self.recipient_payload['player']}
        recipient_belief_response = requests.request("GET", self.url + '/belief/recipient',
                                                     params=recipient_belief_params).json()
        self.assertEqual(recipient_belief_params, recipient_belief_response['data'])
        self.assertTrue(recipient_belief_response['success'])
        self.assertEqual(4, recipient_belief_response['timepoint'])
        self.assertEqual(self.donor_payload['player'], recipient_belief_response['donor'])

    def test_interaction_belief(self):
        interaction_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                     "generation": 0, "player1": self.recipient_payload['player'],
                                     "player2": self.donor_payload['player']}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertTrue(interaction_belief_response['success'])
        self.assertEqual(4, interaction_belief_response['timepoint'])
        self.assertEqual(self.donor_payload['player'], interaction_belief_response['donor'])
        self.assertEqual(self.recipient_payload['player'], interaction_belief_response['recipient'])

    def test_donor_incorrect_community(self):
        donor_belief_params = {"timepoint": 5, "community": self.community_response['id']+1000,
                               "generation": 0, "player": self.donor_payload['player']}
        donor_belief_response = requests.request("GET", self.url + '/belief/donor', params=donor_belief_params).json()
        self.assertEqual(donor_belief_params, donor_belief_response['data'])
        self.assertFalse(donor_belief_response['success'])
        self.assertEqual(donor_belief_response['message'], 'No such community')

    def test_donor_incorrect_generation(self):
        donor_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                               "generation": 2, "player": self.donor_payload['player']}
        donor_belief_response = requests.request("GET", self.url + '/belief/donor', params=donor_belief_params).json()
        self.assertEqual(donor_belief_params, donor_belief_response['data'])
        self.assertFalse(donor_belief_response['success'])
        self.assertEqual(donor_belief_response['message'], 'No such generation for this community')

    def test_donor_no_belief(self):
        donor_belief_params = {"timepoint": 1, "community": self.community_response['id'],
                               "generation": 0, "player": self.donor_payload['player']}
        donor_belief_response = requests.request("GET", self.url + '/belief/donor', params=donor_belief_params).json()
        self.assertEqual(donor_belief_params, donor_belief_response['data'])
        self.assertTrue(donor_belief_response['success'])
        self.assertEqual(-1, donor_belief_response['timepoint'])
        self.assertEqual(-1, donor_belief_response['recipient'])

    def test_donor_incorrect_donor(self):
        donor_belief_params = {"timepoint": 1, "community": self.community_response['id'],
                               "generation": 0, "player": 300}
        donor_belief_response = requests.request("GET", self.url + '/belief/donor', params=donor_belief_params).json()
        self.assertEqual(donor_belief_params, donor_belief_response['data'])
        self.assertFalse(donor_belief_response['success'])
        self.assertEqual(donor_belief_response['message'], 'No such agent in this generation and community')

    def test_recipient_incorrect_community(self):
        recipient_belief_params = {"timepoint": 5, "community": self.community_response['id']+1000,
                                   "generation": 0, "player": self.recipient_payload['player']}
        recipient_belief_response = requests.request("GET", self.url + '/belief/recipient',
                                                     params=recipient_belief_params).json()
        self.assertEqual(recipient_belief_params, recipient_belief_response['data'])
        self.assertFalse(recipient_belief_response['success'])
        self.assertEqual(recipient_belief_response['message'], 'No such community')

    def test_recipient_incorrect_generation(self):
        recipient_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                   "generation": 2, "player": self.recipient_payload['player']}
        recipient_belief_response = requests.request("GET", self.url + '/belief/recipient',
                                                     params=recipient_belief_params).json()
        self.assertEqual(recipient_belief_params, recipient_belief_response['data'])
        self.assertFalse(recipient_belief_response['success'])
        self.assertEqual(recipient_belief_response['message'], 'No such generation for this community')

    def test_recipient_incorrect_recipient(self):
        recipient_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                   "generation": 0, "player": 122}
        recipient_belief_response = requests.request("GET", self.url + '/belief/recipient',
                                                     params=recipient_belief_params).json()
        self.assertEqual(recipient_belief_params, recipient_belief_response['data'])
        self.assertFalse(recipient_belief_response['success'])
        self.assertEqual(recipient_belief_response['message'], 'No such agent in this generation and community')

    def test_recipient_no_belief(self):
        recipient_belief_params = {"timepoint": 1, "community": self.community_response['id'],
                                   "generation": 0, "player": self.recipient_payload['player']}
        recipient_belief_response = requests.request("GET", self.url + '/belief/recipient',
                                                     params=recipient_belief_params).json()
        self.assertEqual(recipient_belief_params, recipient_belief_response['data'])
        self.assertTrue(recipient_belief_response['success'])
        self.assertEqual(-1, recipient_belief_response['timepoint'])
        self.assertEqual(-1, recipient_belief_response['donor'])

    def test_interaction_incorrect_community(self):
        interaction_belief_params = {"timepoint": 5, "community": self.community_response['id']+100,
                                     "generation": 0, "player1": self.recipient_payload['player'],
                                     "player2": self.donor_payload['player']}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertFalse(interaction_belief_response['success'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertEqual('No such community', interaction_belief_response['message'])

    def test_interaction_incorrect_generation(self):
        interaction_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                     "generation": 77, "player1": self.recipient_payload['player'],
                                     "player2": self.donor_payload['player']}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertFalse(interaction_belief_response['success'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertEqual('No such generation for this community', interaction_belief_response['message'])

    def test_interaction_incorrect_player1_and_player2(self):
        interaction_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                     "generation": 0, "player1": 400,
                                     "player2": 333}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertFalse(interaction_belief_response['success'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertEqual('No such agents for this generation and community', interaction_belief_response['message'])

    def test_interaction_incorrect_player1(self):
        interaction_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                     "generation": 0, "player1": 300,
                                     "player2": self.donor_payload['player']}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertFalse(interaction_belief_response['success'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertEqual('No such agent for player1 for this generation and community', interaction_belief_response['message'])

    def test_interaction_incorrect_player2(self):
        interaction_belief_params = {"timepoint": 5, "community": self.community_response['id'],
                                     "generation": 0, "player1": self.recipient_payload['player'],
                                     "player2": 300}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertFalse(interaction_belief_response['success'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertEqual('No such agent for player2 for this generation and community', interaction_belief_response['message'])

    def test_interaction_no_belief(self):
        interaction_belief_params = {"timepoint": 1, "community": self.community_response['id'],
                                     "generation": 0, "player1": self.recipient_payload['player'],
                                     "player2": self.donor_payload['player']}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        self.assertTrue(interaction_belief_response['success'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertEqual(-1, interaction_belief_response['donor'])
        self.assertEqual(-1, interaction_belief_response['recipient'])
        self.assertEqual(-1, interaction_belief_response['timepoint'])




