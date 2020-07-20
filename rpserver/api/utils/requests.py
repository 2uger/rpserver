"""Requests classes from base Request for differ data type"""
from flask import Request


class JSONRequest(Request):
    def __init__(self, *args, **kwargs):
        super(JSONRequest, self).__init__(args, kwargs)

    def get_json(self, *args, **kwargs):
        return super(JSONRequest, self).get_json(args, kwargs)
