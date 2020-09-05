
def hash_password(password):
    cost_factor = app.config.get('BCRYPT_COST_FACTOR') 
    salt = bcrypt.gensalt(rounds=cost_factor)
    return bcrypt.hashpw(password, salt)


def is_valid_password(password, hash_password):
    return bcrypt.checkpw(password, hash_password)


def encode_access_token(user_id):
    try:
        payloads = {
                'exp': datetime.datetime.utcnow() + datetime.timedelta(day=10),
                'iat': datetime.datetime.utcnow(),
                'sub': user_id
                }
        return jwt.encode(
                payload,
                app.config.get('SECRET_KEY'),
                algorithm='HS256'
                )
    except Exception as e:
        raise e


def decode_access_token(auth_token):
    try:
        payload = jwt.decode(auth_token, app.config.get('SECRET_KEY'))
        return payload['sub']
    except jwt.ExpiredSignatureError:
        return 'Expired'
    except jwt.InvalidTokenError:
        return 'Invalid'


def encode_refresh_token(user_id):
    try:
        payloads = {
                'exp': datetime.datetime.utcnow() + datetime.timedelta(day=10),
                'iat': datetime.datetime.utcnow(),
                'sub': user_id
                }
        return jwt.encode(
                payload,
                app.config.get('SECRET_KEY'),
                algorithm='HS256'
                )
    except Exception as e:
        raise e


def decode_refresh_token(auth_token):
    try:
        payload = jwt.decode(auth_token, app.config.get('SECRET_KEY'))
        return payload['sub']
    except jwt.ExpiredSignatureError:
        return 'Expired'
    except jwt.InvalidTokenError:
        return 'Invalid'
