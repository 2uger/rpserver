from flask import current_app, make_response, request
import jwt


def decode_access_token(access_token):
    jwt.decode(access_token, current_app.config.get('SECRET_KEY'), algorithms=["HS256"])
    return 


def token_auth():
    """Check for ability of access token."""
    return

    access_token = request.headers.get('access_token', None)
    if access_token is None:
        current_app.logger.error('Bad access token')
        return make_response({'err': 'Provide access token'}, 401)

    # If it expired it will return handled error
    decode_access_token(access_token)
    return 
