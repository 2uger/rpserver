"""
All exception that raised in app and user should see
"""

from flask import make_response, g

from jwt import ExpiredSignatureError, InvalidTokenError
from werkzeug.exceptions import NotFound, BadRequest, InternalServerError, HTTPException
from psycopg2 import DatabaseError
from psycopg2.errors import UniqueViolation


def internal_server_error(err: InternalServerError):
    """To handle all other exception type"""
    return make_response({"msg": 'Internal server error'})

def not_found(err: NotFound):
    return make_response({'msg': 'Resource not found'}, 404)

def signature_expired(err: ExpiredSignatureError):
    """Token expiration error"""
    return format_http_error(err, err.text)

def invalid_token(err: InvalidTokenError):
    """Raised when invalid token is provided"""
    return format_http_error(err, err.text)


def unique_violation(err: UniqueViolation):
    return make_response({'msg': 'Same object exists'}, 400)

def invalid_transaction(err: DatabaseError):
    if g.db_connection is not None:
        g.db_connection.rollback()
    return make_response({'msg': 'Invalid transaction'}, 404)
    
def http_exception(err: HTTPException):
    return make_response({'msg': 'HTTPException'})

def base_exception(err: Exception):
    return make_response({'msg': 'Unserved exception'}, 406)


exception_handlers = {InternalServerError: internal_server_error,
                      HTTPException: http_exception,
                      ExpiredSignatureError: signature_expired,
                      InvalidTokenError: invalid_token,
                      DatabaseError: invalid_transaction}
