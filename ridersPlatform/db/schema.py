"""

Database schema(metadata)

"""


from sqlalchemy import (
    Column, Date, ForeignKey, Integer, Boolean,
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


user = Table(
    'user',
    metadata,
    Column('user_id', Integer, primary_key=True),
    Column('name', String(50), nullable=False),
    Column('surname', String(50), nullable=False),
    Column('login_email', String(50), nullable=False),
    Column('age', Integer, nullable=False),
    Column('bio', String(50), nullable=False),
    Column('profile_image_url', String(50), nullable=False),
    Column('hometown', String(50), nullable=False),
    Column('registration_date', Date, nullable=False),
    Column('event', Boolean, nullable=False),
)


relations = Table(
    'user_relations',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('relation_id', Integer, ForeignKey('users.user_id'), nullable=False),
    Column('relation_type', Integer, nullable=False)
)


spot = Table(
    'spot',
    metadata,
    Column('spot_id', Integer, primary_key=True),
    Column('name', String(50), nullable=False),
    Column('location', String(50), nullable=False),
    Column('notes', String(50), nullable=False),
    Column('profile_image_url', String(50), nullable=False)
)

coordinates = Table(
    'coordinates',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('longitude', Float, nullable=False),
    Column('latitude', Float, nullable=False)
)


event = Table(
    'event',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('spot_id', Integer, ForeignKey('spots.spot_id')),
    Column('description', String(50), nullable=False),
    Column('datetime', DateTime, nullable=False)
)

tables = [user, relations, spot, coordinates, event]
tables_classes = [User, UserRelation, Spot, Coordinates, Event]
for i in range(len(tables)):
    mapper(tables_classes[i], tables[i])

