from flask import make_response, request
from sqlalchemy import select, insert

from . import auth_bp
from ..valid_data_schema import PostUserSchema
from .auth_utils import encode_access_token, encode_refresh_token

from rpserver.db.schema import user_table


@auth_bp.route('/add/', methods=['POST'])
def user_registration():
    user_register_data = request.get_json()
    PostUserSchema().load(user_register_data)

    connection = db_connection()
    
    # check if user with the same email
    user_check_query = select([user_table]).where(user_table.c.login_email ==
                                                  user_register_data.get('login_email'))
    connection.execute(user_check_query)

    # adding user to db and then SELECT his id to
    # make authorization token
    user_insert_query = insert([user_table]).values(user_register_data)
    connection.execute(user_insert_query).fetchall()

    # fetch user_id from db to make sure that this is right id
    user_id_query = select([user_table.c.user_id]).where(user_table.c.login_email ==
                                                         user_register_data['login_email'])
    user_id = connection.execute(user_id_query).fetchall()
    refresh_token = encode_refresh_token('user_id')
    access_token = encode_access_token('user_id')
    make_response({'message': 'Registration complete',
                   'refresh_token': refresh_token,
                   'access_token': access_token},
                   200)


