"""
All exception that raised in app and user should see
"""

from flask import make_response

from jwt import ExpiredSignatureError, InvalidTokenError
from werkzeug.exceptions import BadRequest, InternalServerError


def format_http_error(error_cls, message: str = None):
    error_code = error_cls.code
    error_message = {'message': message or error_cls.description}
    return make_response(error_message)


def internal_server_error(err: InternalServerError):
    """To handle all other exception type"""
    return format_http_error(err, err.text)


def signature_expired(err: ExpiredSignatureError):
    """Token expiration error"""
    return format_http_error(err, err.text)


def invalid_token(err: InvalidTokenError):
    """Raised when invalid token is provided"""
    return format_http_error(err, err.text)


def bad_request(err: BadRequest):
    return format_http_error(err, err.text)


exception_handlers = {ExpiredSignatureError: signature_expired,
                      InvalidTokenError: invalid_token,
                      BadRequest: bad_request, 
                      InternalServerError: internal_server_error}
