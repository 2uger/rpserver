"""
Utils for authorization:

: password(enc, dec)
: token authorization(access, refresh)

"""

import datetime

from flask import current_app
import bcrypt
import jwt


def hash_password(password: str):
    cost_factor = current_app.config.get('BCRYPT_COST_FACTOR') 
    salt = bcrypt.gensalt(rounds=cost_factor)
    return bcrypt.hashpw(password, salt)


def is_valid_password(password: str, password_hash: str):
    return bcrypt.checkpw(password, password_hash)


def encode_access_token(user_id):
    payloads = {
            'exp': datetime.datetime.utcnow() + datetime.timedelta(days=1),
            'iat': datetime.datetime.utcnow(),
            'sub': user_id
            }
    return jwt.encode(
            payloads,
            current_app.config.get('SECRET_KEY'),
            algorithm='HS256'
            )
   

def decode_access_token(access_token):
    payload = jwt.decode(access_token, current_app.config.get('SECRET_KEY'))
    return payload['sub']


def encode_refresh_token(user_id):
    payloads = {
            'exp': datetime.datetime.utcnow() + datetime.timedelta(days=100),
            'iat': datetime.datetime.utcnow(),
            'sub': user_id
            }
    return jwt.encode(
            payloads,
            current_app.config.get('SECRET_KEY'),
            algorithm='HS256'
            )


def decode_refresh_token(refresh_token):
    payload = jwt.decode(refresh_token, current_app.config.get('SECRET_KEY'))
    return payload['sub']
