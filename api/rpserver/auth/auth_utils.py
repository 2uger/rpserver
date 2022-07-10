import datetime as dt
import hashlib
import random

from flask import current_app
import jwt

REFRESH_TOKEN_EXP_TIME = lambda : dt.datetime.now() + dt.timedelta(days=20)


def hash_password(seq: str):
    return hashlib.sha256(seq.encode('utf-8')).hexdigest()


def is_valid_password(hashed_password: str, password: str):
    return hashlib.sha256(password.encode('utf-8')).hexdigest() == hashed_password


def encode_access_token(user_id: int):
    payload = {'exp': dt.datetime.utcnow() + dt.timedelta(days=1),
               'iat': dt.datetime.utcnow(),
               'user_id': user_id}
    return jwt.encode(payload,
                      current_app.config.get("SECRET_KEY"),
                      algorithm="HS256")
   

def decode_access_token(access_token):
    return jwt.decode(access_token,
                      current_app.config.get("SECRET_KEY"),
                      algorithms=["HS256"])


def encode_refresh_token(user_id: int):
    payload = {'user_id': user_id,
               'rand': random.random()}
    return jwt.encode(payload,
                      current_app.config.get('SECRET_KEY'),
                      algorithm='HS256')


def decode_refresh_token(refresh_token):
    payload = jwt.decode(refresh_token,
                         current_app.config.get('SECRET_KEY'),
                         algorithms=["HS256"])
    return payload
