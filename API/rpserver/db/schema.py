"""

Database schema(metadata)

"""


from sqlalchemy import (
    Column, Date, ForeignKey, ForeignKeyConstraint, Integer, Boolean,
    String, Table, MetaData, Float, DateTime
)
from sqlalchemy.orm import mapper


from ridersPlatform.db.models import User, UserRelation, Spot, Coordinates, Event


convention = {
    'all_column_names': lambda constraint, table: '_'.join([
        column.name for column in constraint.columns.values()
    ]),

    # Именование индексов
    'ix': 'ix__%(table_name)s__%(all_column_names)s',

    # Именование уникальных индексов
    'uq': 'uq__%(table_name)s__%(all_column_names)s',

    # Именование CHECK-constraint-ов
    'ck': 'ck__%(table_name)s__%(constraint_name)s',

    # Именование внешних ключей
    'fk': 'fk__%(table_name)s__%(all_column_names)s__%(referred_table_name)s',

    # Именование первичных ключей
    'pk': 'pk__%(table_name)s'
}
metadata = MetaData(naming_convention=convention)


user_table = Table(
    'user',
    metadata,
    Column('user_id', Integer, primary_key=True),
    Column('name', String(50), nullable=False),
    Column('surname', String(50), nullable=False),
    Column('login_email', String(50), nullable=False),
    Column('birth_date', Date, nullable=False),
    Column('bio', String(50), nullable=False),
    Column('profile_image_url', String(50), nullable=False),
    Column('hometown', String(50), nullable=False),
    Column('registration_date', Date, nullable=False),
    Column('event', Boolean, nullable=False),
)


def hash_password(password):
    cost_factor = app.config.get('BCRYPT_COST_FACTOR') 
    salt = bcrypt.gensalt(rounds=cost_factor)
    return bcrypt.hashpw(password, salt)


def is_valid_password(password, hash_password):
    return bcrypt.checkpw(password, hash_password)


def encode_auth_token(user_id):
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


def decode_auth_token(auth_token):
    try:
        payload = jwt.decode(auth_token, app.config.get('SECRET_KEY'))
        return payload['sub']
    except jwt.ExpiredSignatureError:
        return 'Expired'
    except jwt.InvalidTokenError:
        return 'Invalid'



relations_table = Table(
    'user_relations',
    metadata,
    Column('user_id', Integer, primary_key=True),
    Column('related_user_id', Integer, nullable=False),
    Column('relation_type', Integer, nullable=False),
    ForeignKeyConstraint(['user_id', 'related_user_id'], ['user.user_id', 'user.user_id'])
)


spot_table = Table(
    'spot',
    metadata,
    Column('spot_id', Integer, primary_key=True),
    Column('name', String(50), nullable=False),
    Column('location', String(50), nullable=False),
    Column('notes', String(50), nullable=False),
    Column('profile_image_url', String(50), nullable=False)
)

coordinates_table = Table(
    'coordinates',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('longitude', Float, nullable=False),
    Column('latitude', Float, nullable=False)
)


event_table = Table(
    'event',
    metadata,
    Column('event_id', Integer, primary_key=True),
    Column('user_id', Integer, nullable=False),
    Column('spot_id', Integer, nullable=False),
    Column('name', String(20), nullable=False),
    Column('description', String(50), nullable=False),
    Column('datetime', DateTime, nullable=False),
    ForeignKeyConstraint(['user_id', 'spot_id'], ['user.user_id', 'spot.spot_id'])
)


#: Table to describe token that logout or
#: just to close services for them.
blacklist_token_table = Table(
        'logout_token',
        metadata,
        Column('user_id', Integer, ForeignKey('user.user_id'), primary_key=True)
        Column('token', String(250), nullable=False, unique=True),
        Column('logout_time', DateTime, nullable=False)
        )
