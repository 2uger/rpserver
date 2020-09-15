"""
All exception that raised in app and user should see
"""


from flask import make_response

from jwt import ExpiredSignatureError , InvalidTokenError
from marshmallow import ValidationError
from werkzeug.exceptions import BadRequest, InternalServerError
from sqlalchemy.exc import ArgumentError


def internal_server_error(exc: InternalServerError):
    """To handle all other exception type"""
    
    make_response({'error': {'message': 'Internal server error'}}, 500)


def valid_data(exc: ValidationError):
    """Marshmallow validation error raise when data is incorrect"""
    make_response({'error': {'message': exc.messages}}, 400)


def signature_expired(exc: ExpiredSignatureError):
    """Token expiration error"""
    make_response({'error': {'message': exc.messages}}, 400)


def invalid_token(exc: InvalidTokenError):
    """Raised when invalid token is provided"""
    make_response({'error': {'message': exc.messages}}, 400)


def bad_request(exc: BadRequest):
    make_response({'error': {'message': exc.messages}}, 400)


def invalid_sql_request(exc: ArgumentError):
    make_response({'error': {'message': exc.messages}}, 500)


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
