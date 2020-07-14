from sqlalchemy import (
    Column, Date, ForeignKey, Integer, Boolean,
    String, Table, MetaData, Float, DateTime
)


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

users_table = Table(
    'users',
    metadata,
    Column('user_id', Integer, primary_key=True),
    Column('name', String, nullable=False),
    Column('surname', String, nullable=False),
    Column('login_email', String, nullable=False),
    Column('age', Integer, nullable=False),
    Column('bio', String, nullable=False),
    Column('profile_image_url', String, nullable=False),
    Column('hometown', String, nullable=False),
    Column('registration_date', Date, nullable=False),
    Column('event', Boolean, nullable=False),
)

relations_table = Table(
    'users_relations',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('relation_id', Integer, ForeignKey('users.user_id'), nullable=False),
    Column('relation_type', Integer, nullable=False)
)

spots_table = Table(
    'spots',
    metadata,
    Column('spot_id', Integer, primary_key=True),
    Column('name', String, nullable=False),
    Column('location', String, nullable=False),
    Column('notes', String, nullable=False),
    Column('profile_image_url', String, nullable=False)
)

coordinates_table = Table(
    'coordinates',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('longitude', Float, nullable=False),
    Column('latitude', Float, nullable=False)
)

events_table = Table(
    'events',
    metadata,
    Column('user_id', Integer, ForeignKey('users.user_id'), primary_key=True),
    Column('spot_id', Integer, ForeignKey('spots.spot_id')),
    Column('description', String, nullable=False),
    Column('datetime', DateTime, nullable=False)
)