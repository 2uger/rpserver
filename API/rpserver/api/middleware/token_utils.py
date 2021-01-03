import datetime

from flask import current_app
import jwt


def decode_access_token(access_token):
    payload = jwt.decode(access_token, current_app.config.get('SECRET_KEY'))
    return payload['sub']
