"""
BaseHandler
"""


from json import JSONDecodeError


from rpserver.db import engine


class BaseHandler:
    def __init__(self, request):
        pass

    def get(self):
        pass

    def post(self):
        pass

    def put(self):
        pass

    def delete(self):
        pass

