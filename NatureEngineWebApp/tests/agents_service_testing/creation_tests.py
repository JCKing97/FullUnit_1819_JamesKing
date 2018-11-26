import requests
import unittest

class StrategyTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_get_strategies(self):
        response = requests.request("GET", self.url + '/strategies').json()
        self.assertTrue(response['success'])
        self.assertEquals(response['status'], 200)
        print(response)


# Run when just started Agents service (with no previous communities
class CommunityTestStart(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_community_creation(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response0['id'], 0)
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response1['id'], 1)
        response2 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response2['id'], 2)
        response3 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response3['id'], 3)
        response4 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response4['id'], 4)


# Run whenever
class CommunityTestUnordered(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_community_creation1(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response1['id'], response0['id']+1)
        response2 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response2['id'], response0['id'] + 2)

    def test_community_creation2(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response1['id'], response0['id']+1)
        response2 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response2['id'], response1['id'] + 1)

    def test_community_creation3(self):
        response0 = requests.request("PUT", self.url + '/community').json()
        response1 = requests.request("PUT", self.url + '/community').json()
        self.assertEquals(response1['id'], response0['id']+1)


# Run when no one else is using the Agents Service
class GenerationIncorrectCommunityTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_generation_creation_incorrect_community(self):
        community_response = requests.request("PUT", self.url + '/community').json()
        payload = {"community": community_response['id']+1, "generation": 0}
        generation_response = requests.request("PUT", self.url + '/generation', json=payload).json()
        self.assertEquals(generation_response['success'], False)


# Run whenever
class GenerationTest(unittest.TestCase):

    def setUp(self):
        self.url = "http://localhost:8080"

    def test_generation_creation1(self):
        community_response = requests.request("GET", self.url).json()
        payload = {"community": community_response['id'], "generation": 0}
        generation_response = requests.request("POST", self.url, json=payload).json()
        self.assertEquals(generation_response['status'], "Good")

    def test_generation_creation2(self):
        community_response = requests.request("GET", self.url).json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("POST", self.url, json=payload1).json()
        self.assertEquals(generation_response1['status'], "Good")
        payload2 = {"community": community_response['id'], "generation": 1}
        generation_response2 = requests.request("POST", self.url, json=payload2).json()
        self.assertEquals(generation_response2['status'], "Good")

    def test_generation_creation_no_repeating_generation(self):
        community_response = requests.request("GET", self.url).json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("POST", self.url, json=payload1).json()
        self.assertEquals(generation_response1['status'], "Good")
        payload2 = {"community": community_response['id'], "generation": 0}
        generation_response2 = requests.request("POST", self.url, json=payload2).json()
        self.assertEquals(generation_response2['status'], "Bad")

    def test_generation_creation2_no_repeating_generation(self):
        community_response = requests.request("GET", self.url).json()
        payload1 = {"community": community_response['id'], "generation": 0}
        generation_response1 = requests.request("POST", self.url, json=payload1).json()
        self.assertEquals(generation_response1['status'], "Good")
        payload2 = {"community": community_response['id'], "generation": 0}
        generation_response2 = requests.request("POST", self.url, json=payload2).json()
        self.assertEquals(generation_response2['status'], "Bad")
        payload3 = {"community": community_response['id'], "generation": 1}
        generation_response3 = requests.request("POST", self.url, json=payload3).json()
        self.assertEquals(generation_response3['status'], "Good")