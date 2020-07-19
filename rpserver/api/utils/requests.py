"""Requests classes from base Request for differ data type"""
from flask import Request


class JSONrequest(Request):
    def __init__(self, *args, **kwargs):
        super(JSONrequest, self).__init__(args, kwargs)

    def get_json(self, *args, **kwargs):
        return super(JSONrequest, self).get_json(args, kwargs)
