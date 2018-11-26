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


# Run when just started Agents service (with no previous communities
class CommunityTestStart(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_community_creation(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response0['id'], 0)
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response1['id'], 1)
        response2 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response2['id'], 2)
        response3 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response3['id'], 3)
        response4 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response4['id'], 4)


# Run whenever
class CommunityTestUnordered(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_community_creation1(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response1['id'], response0['id']+1)
        response2 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response2['id'], response0['id'] + 2)

    def test_community_creation2(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response1['id'], response0['id']+1)
        response2 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response2['id'], response1['id'] + 1)

    def test_community_creation3(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEqual(response1['id'], response0['id']+1)


# Run when no one else is using the Agents Service
class GenerationIncorrectCommunityTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_generation_creation_incorrect_community(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        payload = {"community": community_response['id']+1, "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=payload)
        self.assertFalse(generation_response.json()['success'])


# Run whenever
class GenerationTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_generation_creation1(self):
        community_response = requests.request("PUT", self.url+'/community').json()
        payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=payload).json()
        self.assertTrue(generation_response['success'])

    def test_generation_creation2(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("PUT", self.url + '/generation', json=payload1).json()
        self.assertTrue(generation_response1['success'])
        payload2 = {"community": community_response['id'], "generation": 1}
        generation_response2 = requests.request("PUT", self.url + '/generation', json=payload2).json()
        self.assertTrue(generation_response2['success'])

    def test_generation_creation_no_repeating_generation(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("PUT", self.url + '/generation', json=payload1).json()
        self.assertTrue(generation_response1['success'])
        payload2 = {"community": community_response['id'], "generation": 0}
        generation_response2 = requests.request("PUT", self.url + '/generation', json=payload2).json()
        self.assertFalse(generation_response2['success'])

    def test_generation_creation2_no_repeating_generation(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("PUT", self.url + '/generation', json=payload1).json()
        self.assertTrue(generation_response1['success'])
        payload2 = {"community": community_response['id'], "generation": 0}
        generation_response2 = requests.request("PUT", self.url + '/generation', json=payload2).json()
        self.assertFalse(generation_response2['success'])
        payload3 = {"community": community_response['id'], "generation": 1}
        generation_response3 = requests.request("PUT", self.url + '/generation', json=payload3).json()
        self.assertTrue(generation_response3['success'])