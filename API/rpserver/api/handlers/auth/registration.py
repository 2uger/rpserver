from flask import g, make_response, request
from sqlalchemy import select, insert

from . import auth_bp
from ..valid_data_schema import PostUserSchema
from .auth_utils import encode_access_token, encode_refresh_token

from rpserver.db.schema import user_table


@auth_bp.route('/registration/', methods=['POST'])
def user_registration():
    user_register_data = request.get_json()
    print(user_register_data)
    PostUserSchema().load(user_register_data)
    connection = g.db_connection
    # Check if there is user with the same email
    user_check_query = user_table.select().where(user_table.c.login_email ==
                                                 user_register_data.get('login_email'))
    connection.execute(user_check_query)

    user_insert_query = user_table.insert().values(user_register_data)
    connection.execute(user_insert_query)

    return make_response({'message': 'Registration completed'}, 200)


