"""
Utils for authorization:

: password(hash, check)
: token authorization(access, refresh)

"""

import datetime
import hashlib
import random

from flask import current_app
import jwt


def hash_sequence(seq: str):
    return hashlib.sha256(seq.encode('utf-8')).hexdigest()


def is_valid_close_key(hashed_close_key: str, close_key: bytes):
    return hashlib.sha256(close_key.encode('utf-8')).hexdigest() == hashed_close_key


def encode_access_token(uuid: str):
    payload = {
            'exp': datetime.datetime.utcnow() + datetime.timedelta(days=1),
            'iat': datetime.datetime.utcnow(),
            'sub': uuid
            }
    return jwt.encode(
            payload,
            current_app.config.get("SECRET_KEY"),
            algorithm="HS256"
            )
   

def decode_access_token(access_token):
    payload = jwt.decode(access_token, current_app.config.get("SECRET_KEY"), algorithms=["HS256"])
    return payload


def encode_refresh_token(uuid: str):
    payload = {
            'sub': uuid,
            'rand': random.random()
            }
    return jwt.encode(
            payload,
            current_app.config.get('SECRET_KEY'),
            algorithm='HS256'
            )


def decode_refresh_token(refresh_token):
    payload = jwt.decode(refresh_token, current_app.config.get('SECRET_KEY'), algorithms=["HS256"])
    return payload
