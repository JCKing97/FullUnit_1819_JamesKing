import unittest
import requests


class BeliefsDonorRecipientInteractionTesting(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"
        self.community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(self.community_response['success'])
        self.gen_payload = {"community": self.community_response['id'], "generation": 0}
        self.generation_response = requests.request("POST", self.url + '/generation', json=self.gen_payload).json()
        self.assertTrue(self.generation_response['success'])
        self.donor_payload = {"strategy": "Defector", "options": [], "community": self.community_response['id'],
                              "generation": 0, "player": 0}
        self.donor_response = requests.request("POST", self.url + '/agent', json=self.donor_payload).json()
        self.assertEqual(self.donor_response['data'], self.donor_payload)
        self.assertTrue(self.donor_response['success'])
        self.recipient_payload = {"strategy": "Cooperator", "options": [], "community": self.community_response['id'],
                                  "generation": 0, "player": 1}
        self.recipient_response = requests.request("POST", self.url + '/agent', json=self.recipient_payload).json()
        self.assertEqual(self.recipient_response['data'], self.recipient_payload)
        self.assertTrue(self.recipient_response['success'])
        self.percept_payload = {"community": self.community_response['id'], "generation": 0,
                           "donor": self.donor_payload['player'],
                           "recipient": self.recipient_payload['player'], "timepoint": 4}
        self.percept_response = requests.request("POST", self.url + '/percept/interaction', json=self.percept_payload).json()
        self.assertEqual(self.percept_payload, self.percept_response['data'])
        self.assertTrue(self.percept_response['success'])

    def test_donor_belief(self):
        donor_belief_params = {"timepoint": 4, "community": self.community_response['id'],
                               "generation": 0, "player": self.donor_payload['player']}
        donor_belief_response = requests.request("GET", self.url + '/belief/donor', params=donor_belief_params).json()
        print(donor_belief_response['interactions'])
        self.assertEqual(donor_belief_params, donor_belief_response['data'])
        self.assertTrue(donor_belief_response['success'])
        self.assertEqual([{'timepoints': [4], 'recipient': self.recipient_payload['player']}],
                         donor_belief_response['interactions'])

    def test_recipient_belief(self):
        recipient_belief_params = {"timepoint": 4, "community": self.community_response['id'],
                                   "generation": 0, "player": self.recipient_payload['player']}
        recipient_belief_response = requests.request("GET", self.url + '/belief/recipient',
                                                     params=recipient_belief_params).json()
        print(recipient_belief_response['interactions'])
        self.assertEqual(recipient_belief_params, recipient_belief_response['data'])
        self.assertTrue(recipient_belief_response['success'])
        self.assertEqual([{'timepoints': [4], 'donor': self.donor_payload['player']}],
                         recipient_belief_response['interactions'])

    def test_interaction_belief(self):
        interaction_belief_params = {"timepoint": 4, "community": self.community_response['id'],
                                     "generation": 0, "player1": self.recipient_payload['player'],
                                     "player2": self.donor_payload['player']}
        interaction_belief_response = requests.request("GET", self.url + '/belief/interaction',
                                                       params=interaction_belief_params).json()
        print(interaction_belief_response['interactions'])
        self.assertEqual(interaction_belief_params, interaction_belief_response['data'])
        self.assertTrue(interaction_belief_response['success'])
        self.assertEqual([{'timepoints': [4], 'recipient': self.recipient_payload['player'],
                           'donor': self.donor_payload['player']}], interaction_belief_response['interactions'])

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
        self.assertEqual([], donor_belief_response['interactions'])

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
        self.assertEqual([], recipient_belief_response['interactions'])

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
        self.assertEqual([], interaction_belief_response['interactions'])
