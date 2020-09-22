"""

Database schema(metadata)

"""


from sqlalchemy import (
    Column, Date, ForeignKey, ForeignKeyConstraint, Integer, Boolean,
    String, Table, MetaData, DateTime
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


relations_table = Table(
    'user_relations',
    metadata,
    Column('user_id', Integer, ForeignKey('user.user_id'), primary_key=True),
    Column('related_user_id', Integer, ForeignKey('user.user_id'), nullable=False),
    Column('relation_type', Integer, nullable=False),
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


# event_table = Table(
#     'event',
#     metadata,
#     Column('event_id', Integer, primary_key=True),
#     Column('user_id', Integer, nullable=False),
#     Column('spot_id', Integer, nullable=False),
#     Column('name', String(20), nullable=False),
#     Column('description', String(50), nullable=False),
#     Column('datetime', DateTime, nullable=False),
#     ForeignKeyConstraint(('user_id', 'spot_id'), ('user.user_id', 'spot.spot_id'))
# )

# event_table = Table(
#     'event',
#     metadata
# )
#: Table to describe token that logout or
#: just to close services for them.
# blacklist_token_table = Table(
#         'logout_token',
#         metadata,
#         Column('user_id', Integer, ForeignKey('user.user_id'), primary_key=True),
#         Column('token', String(250), nullable=False, unique=True),
#         Column('logout_time', DateTime, nullable=False)
#         )
