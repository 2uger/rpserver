import datetime

from flask import current_app
import jwt


def encode_access_token(rider_id: int):
    payload = {
            'exp': datetime.datetime.utcnow() + datetime.timedelta(days=1),
            'iat': datetime.datetime.utcnow(),
            'sub': rider_id
            }
    return jwt.encode(
            payload,
            current_app.config.get('SECRET_KEY'),
            algorithm='HS256'
            )
