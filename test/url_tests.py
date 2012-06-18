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
		self.new_paper_url = self.paper_url + '0'


	def test_get_on_root_returns_html_hello_world(self):
		resp = requests.get(self.base_url)
		self.assertEqual(resp.content, "<html><body>Hello, new world</body></html>")


	def test_get_on_paper_returns_id_in_html(self):
		for id in 1,2,3:
			resp = self.get_paper_with_id_and_header(id, headers={})
			self.assertEqual(resp.status_code, 200)
			self.assertEqual(resp.content, "<html><body>" + str(id) + "</body></html>")



	""" ********* Helpers *********"""
	def get_paper_with_id_and_header(self, id, headers=None):
		if headers == None:
			headers = self.json_headers
		return requests.get(self.paper_url + str(id), headers=headers)

if __name__ == "__main__":
	suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestPaperAPI)
	unittest.TextTestRunner.run(suite)
