from flask import make_response, request
from sqlalchemy import select

from . import auth_bp
from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         is_valid_password)

from rpserver.db.schema import user_table
from ..valid_data_schema import LoginUserSchema


@auth_bp.route('/login/', methods=['POST'])
def user_login():

    """
    User login to send access_token and refresh_token back 
    """

    login_data = request.get_json()
    LoginUserSchema().loads(login_data)
    db_connection = connect_db()
    user_id_query = select([user_table]).where(user_table.c.login_email ==
            login_data.get('login_email'))
    user = db_connection.execute(user_id_query).fetchall()
    if is_valid_password(login_data.get('password'), user.get('password')):
        make_response({'message': 'Invalid login or password'}, 400)
    refresh_token = encode_access_token(user.get('user_id'))
    access_token = encode_access_token(user.get('user_id'))
    if access_token and refresh_token:
        response = {'message': 'Login succesfully',
                    'refresh_token': refresh_token,
                    'auth_token': access_token}
        make_response(response, 200)
    else:
        make_response({'message': 'Try again'}, 500)

