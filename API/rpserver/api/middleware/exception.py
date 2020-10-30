"""
All exception that raised in app and user should see
"""

from flask import make_response

from jwt import ExpiredSignatureError, InvalidTokenError
from marshmallow import ValidationError
from werkzeug.exceptions import HTTPException, BadRequest, InternalServerError, NotFound
from sqlalchemy.exc import ArgumentError


def format_http_error(error_cls, message: str = None):
    # error_code = error_cls.code
    error_message = {'message': message or error_cls.description}
    return error_message

def exception(err):
    """ If no others handlers has been accept """
    return format_http_error(err)


def internal_server_error(err: InternalServerError):
    """To handle all other exception type"""
    return format_http_error(err)


def valid_data(err):
    """Marshmallow validation error raise when data is incorrect"""
    return format_http_error(err, err.messages)


def signature_expired(err: ExpiredSignatureError):
    """Token expiration error"""
    return format_http_error(err)


def invalid_token(err: InvalidTokenError):
    """Raised when invalid token is provided"""
    return format_http_error(err)


def not_found(err: NotFound):
    return format_http_error(err) 


def bad_request(err: BadRequest):
    return format_http_error(err)


exception_handlers = {ValidationError: valid_data,
                      ExpiredSignatureError: signature_expired,
                      InvalidTokenError: invalid_token,
                      BadRequest: bad_request,
                      InternalServerError: internal_server_error,
                      NotFound: not_found,
                      HTTPException: exception}
