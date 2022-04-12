from flask import current_app, make_response, request

from .auth_utils import decode_access_token


def token_auth():
    """Check for ability of access token."""
    current_app.logger.info(f'Incoming request: {request}')
    return
    access_token = request.headers.get('access-token', None)
    if not access_token:
        return make_response({'err': 'provide credentials'}, 401)

    payload = decode_access_token(access_token)
    request.user_id = payload['user_id']
    current_app.logger.info('Request from ', request.user_id)
    return 
