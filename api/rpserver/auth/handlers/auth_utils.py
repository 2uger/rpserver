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


def hash_seq(password: str):
    password_hash = hashlib.sha256()
    password_hash.update(password.encode('utf-8'))
    return password_hash.hexdigest()

def hash_by_seq(sequence: str, close_key: str):
    pass


def is_valid_seq(password: str, hashed_password: bytes):
    password_hash = hashlib.sha256()
    password_hash.update(password.encode('utf-8'))
    return password_hash.hexdigest() == hashed_password


def encode_access_token(rider_id: int):
    payload = {
            'exp': datetime.datetime.utcnow() + datetime.timedelta(days=1),
            'iat': datetime.datetime.utcnow(),
            'sub': rider_id
            }
    return jwt.encode(
            payload,
            current_app.config.get("SECRET_KEY"),
            algorithm="HS256"
            )
   

def decode_access_token(access_token):
    payload = jwt.decode(access_token, current_app.config.get("SECRET_KEY"), algorithms=["HS256"])
    return payload


def encode_refresh_token(rider_id: int):
    payload = {
            'sub': rider_id,
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
