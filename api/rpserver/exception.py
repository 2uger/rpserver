from flask import make_response, g

from jwt import ExpiredSignatureError, InvalidTokenError
from werkzeug.exceptions import NotFound, InternalServerError, HTTPException, MethodNotAllowed
from psycopg2 import DatabaseError
from psycopg2.errors import UniqueViolation
from marshmallow.exceptions import ValidationError


def internal_server_error(err: InternalServerError):
    """To handle all other exception type"""
    return make_response({"err": "Internal server error"}, 500)


def not_found(err: NotFound):
    return make_response({"err": "Resource not found"}, 404)


def method_not_allowed(err: MethodNotAllowed):
    return make_response({'err': 'Method not allowed'}, 405)


def http_exception(err: HTTPException):
    return make_response({'err': 'HTTPException'})


def base_exception(err: Exception):
    return make_response({'err': 'Unserved exception'}, 406)


# Token errors
def signature_expired(err: ExpiredSignatureError):
    """Token expiration error"""
    return make_response({"err": "Token expired"}, 423)


def invalid_token(err: InvalidTokenError):
    """Raised when invalid token is provided"""
    return make_response({"err": "Token invalid"}, 423)


# Database errors
def unique_violation(err: UniqueViolation):
    return make_response({'err': str(err)}, 400)


def invalid_transaction(err: DatabaseError):
    if g.db_connection is not None:
        g.db_connection.rollback()
    return make_response({'err': 'Invalid transaction'}, 404)


def validation_error(err: ValidationError):
    return make_response(err.messages, 404)
    

exception_handlers = {InternalServerError: internal_server_error,
                      NotFound: not_found,
                      MethodNotAllowed: method_not_allowed,
                      HTTPException: http_exception,
                      Exception: base_exception,
                      ExpiredSignatureError: signature_expired,
                      InvalidTokenError: invalid_token,
                      UniqueViolation: unique_violation,
                      DatabaseError: invalid_transaction,
                      ValidationError: validation_error}
