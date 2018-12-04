import requests
import unittest


class StrategyTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_get_strategies(self):
        response = requests.request("GET", self.url + '/strategies').json()
        self.assertTrue(response['success'])
        self.assertEqual(response['status'], 200)
        print(response)


# # Run when just started Agents service (with no previous communities
# class CommunityTestStart(unittest.TestCase):
#
#     def setUp(self):
#         self.url = "http://localhost:8080"
#
#     def test_community_creation(self):
#         response0 = requests.request("POST", self.url + '/community').json()
#         self.assertEqual(response0['id'], 0)
#         response1 = requests.request("POST", self.url + '/community').json()
#         self.assertEqual(response1['id'], 1)
#         response2 = requests.request("POST", self.url + '/community').json()
#         self.assertEqual(response2['id'], 2)
#         response3 = requests.request("POST", self.url + '/community').json()
#         self.assertEqual(response3['id'], 3)
#         response4 = requests.request("POST", self.url + '/community').json()
#         self.assertEqual(response4['id'], 4)


# Run whenever
class CommunityTestUnordered(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_community_creation1(self):
        response0 = requests.request("POST", self.url + '/community').json()
        response1 = requests.request("POST", self.url + '/community').json()
        self.assertEqual(response1['id'], response0['id']+1)
        response2 = requests.request("POST", self.url + '/community').json()
        self.assertEqual(response2['id'], response0['id'] + 2)

    def test_community_creation2(self):
        response0 = requests.request("POST", self.url + '/community').json()
        response1 = requests.request("POST", self.url + '/community').json()
        self.assertEqual(response1['id'], response0['id']+1)
        response2 = requests.request("POST", self.url + '/community').json()
        self.assertEqual(response2['id'], response1['id'] + 1)

    def test_community_creation3(self):
        response0 = requests.request("POST", self.url + '/community').json()
        response1 = requests.request("POST", self.url + '/community').json()
        self.assertEqual(response1['id'], response0['id']+1)


# Run whenever
class GenerationTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_generation_creation1(self):
        community_response = requests.request("POST", self.url+'/community').json()
        payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=payload).json()
        self.assertTrue(generation_response['success'])

    def test_generation_creation2(self):
        community_response = requests.request("POST", self.url + '/community').json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("POST", self.url + '/generation', json=payload1).json()
        self.assertTrue(generation_response1['success'])
        payload2 = {"community": community_response['id'], "generation": 1}
        generation_response2 = requests.request("POST", self.url + '/generation', json=payload2).json()
        self.assertTrue(generation_response2['success'])

    def test_generation_creation_no_repeating_generation(self):
        community_response = requests.request("POST", self.url + '/community').json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("POST", self.url + '/generation', json=payload1).json()
        self.assertTrue(generation_response1['success'])
        payload2 = {"community": community_response['id'], "generation": 0}
        generation_response2 = requests.request("POST", self.url + '/generation', json=payload2).json()
        self.assertFalse(generation_response2['success'])
        self.assertEqual(generation_response2['message'], 'This community already has a generation with this id')

    def test_generation_creation2_no_repeating_generation(self):
        community_response = requests.request("POST", self.url + '/community').json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("POST", self.url + '/generation', json=payload1).json()
        self.assertTrue(generation_response1['success'])
        payload2 = {"community": community_response['id'], "generation": 0}
        generation_response2 = requests.request("POST", self.url + '/generation', json=payload2).json()
        self.assertFalse(generation_response2['success'])
        self.assertEqual(generation_response2['message'], 'This community already has a generation with this id')
        payload3 = {"community": community_response['id'], "generation": 1}
        generation_response3 = requests.request("POST", self.url + '/generation', json=payload3).json()
        self.assertTrue(generation_response3['success'])

    def test_generation_creation_incorrect_community(self):
        community_response = requests.request("POST", self.url + '/community').json()
        payload = {"community": community_response['id']+10000, "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=payload).json()
        self.assertFalse(generation_response['success'])
        self.assertEqual(generation_response['message'], 'No such community')


class AgentsTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_agent_creation1(self):
        community_response = requests.request("POST", self.url+'/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        agent_response = requests.request("POST", self.url + '/agent', json=agent_payload).json()
        print(agent_response)
        self.assertEqual(agent_response['data'], agent_payload)
        self.assertTrue(agent_response['success'])

    def test_agent_creation2(self):
        community_response = requests.request("POST", self.url+'/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload1 = {"strategy": "Defector", "options": [], "community": community_response['id'],
                         "generation": 0, "player": 0}
        agent_response1 = requests.request("POST", self.url + '/agent', json=agent_payload1).json()
        print(agent_response1)
        self.assertEqual(agent_response1['data'], agent_payload1)
        self.assertTrue(agent_response1['success'])
        agent_payload2 = {"strategy": "Standing Discriminator", "options": ["trusting"],
                          "community": community_response['id'], "generation": 0, "player": 1}
        agent_response2 = requests.request("POST", self.url + '/agent', json=agent_payload2).json()
        print(agent_response2)
        self.assertEqual(agent_response2['data'], agent_payload2)
        self.assertTrue(agent_response2['success'])

    def test_agent_creation_incorrect_community(self):
        community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload = {"strategy": "Defector", "options": [], "community": community_response['id']+10000,
                          "generation": 0, "player": 0}
        agent_response = requests.request("POST", self.url + '/agent', json=agent_payload).json()
        print(agent_response)
        self.assertEqual(agent_response['data'], agent_payload)
        self.assertFalse(agent_response['success'])
        self.assertEqual(agent_response['message'], 'No such community')

    def test_agent_creation_incorrect_generation(self):
        community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload = {"strategy": "Cooperator", "options": [], "community": community_response['id'],
                          "generation": 3, "player": 0}
        agent_response = requests.request("POST", self.url + '/agent', json=agent_payload).json()
        print(agent_response)
        self.assertEqual(agent_response['data'], agent_payload)
        self.assertFalse(agent_response['success'])
        self.assertEqual(agent_response['message'], 'No such generation for this community')

    def test_agent_creation_incorrect_strategy(self):
        community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload = {"strategy": "defector", "options": [], "community": community_response['id'],
                         "generation": 3, "player": 0}
        agent_response = requests.request("POST", self.url + '/agent', json=agent_payload).json()
        print(agent_response)
        self.assertEqual(agent_response['data'], agent_payload)
        self.assertFalse(agent_response['success'])
        self.assertEqual(agent_response['message'], 'No such strategy')

    def test_agent_creation_incorrect_options(self):
        community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload = {"strategy": "Defector", "options": ["hey"], "community": community_response['id'],
                         "generation": 3, "player": 0}
        agent_response = requests.request("POST", self.url + '/agent', json=agent_payload).json()
        print(agent_response)
        self.assertEqual(agent_response['data'], agent_payload)
        self.assertFalse(agent_response['success'])
        self.assertEqual(agent_response['message'], 'No such strategy')

    def test_agent_creation_duplicate_player_id(self):
        community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        agent_payload1 = {"strategy": "Defector", "options": [], "community": community_response['id'],
                          "generation": 0, "player": 0}
        agent_response1 = requests.request("POST", self.url + '/agent', json=agent_payload1).json()
        print(agent_response1)
        self.assertEqual(agent_response1['data'], agent_payload1)
        self.assertTrue(agent_response1['success'])
        agent_payload2 = {"strategy": "Standing Discriminator", "options": ["trusting"],
                          "community": community_response['id'], "generation": 0, "player": 0}
        agent_response2 = requests.request("POST", self.url + '/agent', json=agent_payload2).json()
        print(agent_response2)
        self.assertEqual(agent_response2['data'], agent_payload2)
        self.assertFalse(agent_response2['success'])
        self.assertEqual(agent_response2['message'], 'Player ID already taken for this community and generation')

    def test_agent_creation_all_strategies(self):
        community_response = requests.request("POST", self.url + '/community').json()
        self.assertTrue(community_response['success'])
        gen_payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url + '/generation', json=gen_payload).json()
        self.assertTrue(generation_response['success'])
        response = requests.request("GET", self.url + '/strategies').json()
        self.assertTrue(response['success'])
        id = 0
        for strategy in response['strategies']:
            agent_payload = {"strategy": strategy['name'], "options": strategy['options'],
                              "community": community_response['id'], "generation": 0, "player": id}
            agent_response = requests.request("POST", self.url + '/agent', json=agent_payload).json()
            self.assertTrue(agent_response['success'])
            self.assertEqual(agent_response['data'], agent_payload)
            id+=1
