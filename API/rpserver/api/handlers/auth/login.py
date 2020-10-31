from flask import make_response, request, g
from sqlalchemy import select

from . import auth_bp
from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         is_valid_password)

from rpserver.db.schema import user_table
from ..valid_data_schema import LoginUserSchema


@auth_bp.route('/login/', methods=['POST'])
def user_login():
    """ User login to send access_token and refresh_token back """

    login_data = request.get_json()
    print(login_data)
    LoginUserSchema().load(login_data)
    db_connection = g.db_connection
    print(login_data)
    user_id_query = user_table.select().where(user_table.c.login_email ==
                                              login_data.get('login_email'))
    user_data = db_connection.execute(user_id_query).fetchone()


    #if not (is_valid_password(login_data.get('password'), user_data.get('password')) and 
    #        login_data.get('login_email') == user_data.get('login_email')):
    #    return make_response({'message': 'Invalid login or password'}, 400)


    refresh_token = encode_refresh_token(user_data[0]).decode()
    access_token = encode_access_token(user_data[0]).decode()
    if access_token and refresh_token:
        response = {'refresh_token': refresh_token,
                    'auth_token': access_token}
        return make_response(response, 200)
    else:
        return make_response({'message': 'Try again'}, 500)

