"""
All exception that raised in app and user should see
"""


from flask import make_response

from jwt import ExpiredSignatureError , InvalidTokenError
from marshmallow import ValidationError
from werkzeug.exceptions import BadRequest, InternalServerError
from sqlalchemy.exc import ArgumentError


def format_http_error():
    pass


def internal_server_error(err InternalServerError):
    """To handle all other exception type"""
    return format_http_error(err, err.text)


def valid_data(err ValidationError):
    """Marshmallow validation error raise when data is incorrect"""
    return format_http_error(HTTPBadRequest, 
                             'Request validation has failed', 
                             err.message)


def signature_expired(err ExpiredSignatureError):
    """Token expiration error"""
    return format_http_error(err, err.text)


def invalid_token(err InvalidTokenError):
    """Raised when invalid token is provided"""
    return format_http_error(err, err.text)


def bad_request(err BadRequest):
    return format_http_error(err, err.text)


exception_list = [ValidationError,
                  ExpiredSignatureError,
                  InvalidTokenError,
                  BadRequest,
                  ArgumentError]


handle_exception = [valid_data,
                    signature_expired,
                    invalid_token,
                    bad_request,
                    invalid_sql_request]
