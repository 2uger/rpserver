"""
Flask middleware for 
-before_request
-after_request
-loggin
"""


from flask import make_response, request
import jwt

from rpserver.api.handlers.auth.auth_utils import decode_access_token


def jwt_token_authorization():
    if not request.headers.get('access_token'):
        make_response({'error': {'message': 'No access_token'}}, 401)
    try:
        access_token = request.header.get('access_token')
        user_id = decode_access_token(access_token)
    except jwt.ExpiredSignatureError:
        make_response({'error': {'message': 'Token expired'}}, 401)