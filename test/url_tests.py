# -*- coding: utf-8 -*-

#!/usr/bin/python
import requests
import unittest
import time

class TestPaperAPI(unittest.TestCase):

	def setUp(self):
		self.base_url = "http://localhost:8888"
		self.paper_url = "http://localhost:8888/paper/"
		self.json_headers ={"Content-Type" : "application/json", "Accept" : "application/json"}
		self.new_paper = {"title": "ABC"}
		self.new_paper2 =  {"title": "DEF"}


	def test_get_on_root_returns_html_hello_world(self):
		resp = requests.get(self.base_url)
		self.assertEqual(resp.content, "<html><body>Hello, new world</body></html>")


	def test_get_on_paper_returns_id_in_html(self):
		for id in 1,2,3:
			resp = self.get_paper_with_id_and_header(id, headers={})
			self.assertEqual(resp.status_code, 200)
			self.assertEqual(resp.content, "<html><body>" + str(id) + "</body></html>")


	def test_get_on_paper_returns_id_in_json(self):
		for id in 1,2,3:
			resp = self.get_paper_with_id_and_header(id)
			self.assertEqual(resp.status_code, 200)
			self.assertEqual(resp.content, '{"id":' + '"' + str(id) + '",'\
					'"title":'+ '"' + str(id) + '"}')


	def test_put__new_paper(self):
		url = self.paper_url + '0'
		# delete it first if present
		requests.delete(url)

		resp = requests.put(url, data=self.new_paper, headers=self.json_headers)
		self.assertEqual(resp.status_code, 200)
		self.assertEqual(resp.content, '{"id":"0", "title":"ABC"}')

		# Test durability
		resp2 = requests.get(url)


	def test_delete_paper(self):
		# create it first
		url = self.paper_url + '0'

		resp = requests.put(url, data=self.new_paper, headers=self.json_headers)
		self.assertEqual(resp.status_code, 200)

		resp1 = requests.delete(url)
		self.assertEqual(resp1.status_code, 204)

		# test if durable
		resp2 = requests.get(url)
		self.assertEqual(resp2.status_code, 404)


	""" ********* Helpers *********"""
	def get_paper_with_id_and_header(self, id, headers=None):
		if headers == None:
			headers = self.json_headers
		return requests.get(self.paper_url + str(id), headers=headers)

if __name__ == "__main__":
	suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestPaperAPI)
	unittest.TextTestRunner.run(suite)
