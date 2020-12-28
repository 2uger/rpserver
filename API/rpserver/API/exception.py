"""
All exception that raised in app and user should see
"""

from flask import make_response, g

from jwt import ExpiredSignatureError, InvalidTokenError
from werkzeug.exceptions import BadRequest, InternalServerError, HTTPException
from psycopg2 import DatabaseError


def format_http_error(error_cls, message: str = None):
    error_code = error_cls.code
    error_message = {'message': message or error_cls.description}
    return make_response(error_message)

def internal_server_error(err: InternalServerError):
    """To handle all other exception type"""
    return make_response({"msg": InternalServerError})

def signature_expired(err: ExpiredSignatureError):
    """Token expiration error"""
    return format_http_error(err, err.text)

def invalid_token(err: InvalidTokenError):
    """Raised when invalid token is provided"""
    return format_http_error(err, err.text)

def invalid_transaction(err: DatabaseError):
    if g.db_connection is not None:
        g.db_connection.rollback()
    return make_response({"msg": "Invalid transaction"}, 404)
    
def http_exception(err: HTTPException):
    return make_response({"msg": Exception})


exception_handlers = {HTTPException: http_exception, 
                      InternalServerError: internal_server_error}
