from flask import make_response, request
import jwt

from rpserver.api.handlers.auth.auth_utils import decode_access_token


def token_authorization():
    """ Check for ability of access token """
    
    if not request.headers.get('access_token'):
        return make_response({'error': {'message': 'No access_token'}}, 401)
    try:
        access_token = request.headers.get('access_token')
        user_id = decode_access_token(access_token)
    except jwt.ExpiredSignatureError:
        return make_response({'error': {'message': 'Token expired'}}, 401)
