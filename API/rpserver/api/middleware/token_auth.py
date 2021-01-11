from flask import current_app, make_response, request
import jwt

from .token_utils import decode_access_token


def token_auth():
    """ Check for ability of access token """

    access_token = request.headers.get('access_token', None)
    if access_token is None:
        current_app.logger.error('Bad access token')
        return make_response({'error': {'message': 'Provide access token'}}, 401)

    # If it expired it will return handled error
    decode_access_token(access_token)
    return 

    #TODO: check if middleware calls after db connection()
