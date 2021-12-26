from flask import current_app, make_response, request
import jwt

from .auth_utils import decode_access_token


def token_auth():
    """Check for ability of access token."""
    return
    access_token = request.headers.get('access-token', None)
    if access_token is None:
        return make_response({'err': 'provide credentials'}, 401)

    payload = decode_access_token(access_token)
    request.user_id = payload['user_id']
    print('Request from ', request.user_id)
    return 
