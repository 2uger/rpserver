from flask import make_response, request
import jwt

from .token_utils import decode_access_token


def jwt_token_authorization():
    """ Check for ability of access token """

    access_token = request.headers.get('access_token', None)
    if access_token is None:
        return make_response({'error': {'message': 'Provide access token'}}, 401)

    decode_access_token(access_token)
    return 

    #TODO: check if middleware calls after db connection()
