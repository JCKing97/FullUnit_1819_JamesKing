import requests
import unittest

class PerceptTests(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_interaction_percept(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                      "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0, "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4}
        percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertTrue(percept_response['success'])

    def test_action_interaction_percept_cooperate(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertTrue(percept_response['success'])

    def test_action_interaction_percept_defect(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "defect"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertTrue(percept_response['success'])

    def test_action_gossip_percept_positive(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": about_payload['player'],
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "positive"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertTrue(percept_response['success'])

    def test_action_gossip_percept_negative(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": about_payload['player'],
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertTrue(percept_response['success'])

    def test_interaction_percept_incorrect_community(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        percept_payload = {"community": community_response['id']+10000, "generation": 0, "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4}
        percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such community')

    def test_interaction_percept_incorrect_generation(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 7, "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4}
        percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such generation for this community')

    def test_interaction_percept_incorrect_donor(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0, "donor": 9,
                           "recipient": recipient_payload['player'], "timepoint": 4}
        percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such donor for this community and generation')

    def test_interaction_percept_incorrect_recipient(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0, "donor": donor_payload['player'],
                           "recipient": 177, "timepoint": 4}
        percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such recipient for this community and generation')

    def test_interaction_percept_incorrect_recipient_and_donor(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0, "donor": 17,
                           "recipient": 177, "timepoint": 4}
        percept_response = requests.request("POST", self.url + '/percept/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such recipient or donor for this community and generation')

    def test_action_interaction_incorrect_community(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id']+10000, "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such community')


    def test_action_interaction_percept_incorrect_generation(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 8,
                           "perceiver": perceiver_payload['player'], "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such generation for this community')

    def test_action_interaction_percept_incorrect_perceiver(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 973, "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver')


    def test_action_interaction_percept_incorrect_donor(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": 777,
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: donor')


    def test_action_interaction_percept_incorrect_recipient(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": donor_payload['player'],
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: recipient')


    def test_action_interaction_percept_incorrect_perceiver_and_donor_and_recipient(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 888, "donor": 777,
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver, donor or recipient')

    def test_action_interaction_percept_incorrect_perceiver_and_donor(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 888, "donor": 777,
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver or donor')

    def test_action_interaction_percept_incorrect_donor_and_recipient(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": 777,
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: donor or recipient')

    def test_action_interaction_percept_incorrect_perceiver_and_recipient(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 888, "donor": donor_payload['player'],
                           "recipient": 999, "timepoint": 4, "action": "cooperate"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver or recipient')

    def test_action_interaction_percept_incorrect_action(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        donor_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        donor_response = requests.request("PUT", self.url + '/agent', json=donor_payload).json()
        self.assertEqual(donor_response['data'], donor_payload)
        self.assertTrue(donor_response['success'])
        recipient_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        recipient_response = requests.request("PUT", self.url + '/agent', json=recipient_payload).json()
        self.assertEqual(recipient_response['data'], recipient_payload)
        self.assertTrue(recipient_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "donor": donor_payload['player'],
                           "recipient": recipient_payload['player'], "timepoint": 4, "action": "failure"}
        percept_response = requests.request("POST", self.url + '/percept/action/interaction', json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'Incorrect action must either be defect or cooperate')

    def test_action_gossip_percept_incorrect_community(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id']+92102, "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": about_payload['player'],
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such community')

    def test_action_gossip_percept_incorrect_generation(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 700,
                           "perceiver": perceiver_payload['player'], "about": about_payload['player'],
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such generation for this community')

    def test_action_gossip_percept_incorrect_perceiver(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 600, "about": about_payload['player'],
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver')

    def test_action_gossip_percept_incorrect_about(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": 11,
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: about')

    def test_action_gossip_percept_incorrect_gossiper(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": about_payload['player'],
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: gossiper')

    def test_action_gossip_percept_incorrect_perceiver_and_about_and_gossiper(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 6786, "about": 7000,
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver, gossiper or about')

    def test_action_gossip_percept_incorrect_perceiver_and_about(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 600, "about": 78,
                           "gossiper":  gossiper_payload['player'], "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver or about')

    def test_action_gossip_percept_incorrect_perceiver_and_gossiper(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": 800, "about": about_payload['player'],
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: perceiver or gossiper')

    def test_action_gossip_percept_incorrect_gossiper_and_about(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": 9287,
                           "gossiper":  300, "timepoint": 4, "gossip": "negative"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'No such agent for: gossiper or about')

    def test_action_gossip_percept_incorrect_gossip(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        perceiver_payload = {"strategy": "Standing Discriminator", "options": ["distrusting"],
                             "community": community_response['id'], "generation": 0, "player": 2}
        perceiver_response = requests.request("PUT", self.url + '/agent', json=perceiver_payload).json()
        self.assertEqual(perceiver_response['data'], perceiver_payload)
        self.assertTrue(perceiver_response['success'])
        about_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        about_response = requests.request("PUT", self.url + '/agent', json=about_payload).json()
        self.assertEqual(about_payload, about_response['data'])
        self.assertTrue(about_response['success'])
        gossiper_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                             "generation": 0, "player": 1}
        gossiper_response = requests.request("PUT", self.url + '/agent', json=gossiper_payload).json()
        self.assertEqual(gossiper_payload, gossiper_response['data'])
        self.assertTrue(gossiper_response['success'])
        percept_payload = {"community": community_response['id'], "generation": 0,
                           "perceiver": perceiver_payload['player'], "about": about_payload['player'],
                           "gossiper": gossiper_payload['player'], "timepoint": 4, "gossip": "failure"}
        percept_response = requests.request("POST", self.url + '/percept/action/gossip',
                                            json=percept_payload).json()
        self.assertEqual(percept_payload, percept_response['data'])
        self.assertFalse(percept_response['success'])
        self.assertEqual(percept_response['message'], 'Incorrect gossip action should be either positive or negative')



