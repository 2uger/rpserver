from flask import make_response, request, g
from psycopg2.extras import DictCursor

from rpserver.rider.schema import LoginUserSchema
from .auth_utils import (
    REFRESH_TOKEN_EXP_TIME,
    encode_access_token, 
    encode_refresh_token,
    is_valid_password
)

from . import auth_bp


@auth_bp.route('/sign-in', methods=['POST'])
def rider_login():
    """Create refresh and access tokens for rider."""

    login_data = request.get_json()

    LoginUserSchema().load(login_data)

    db_connection = g.get('db_connection')
    
    with db_connection.cursor(cursor_factory=DictCursor) as cur:
        rider_query = """SELECT * FROM rider WHERE nickname=%s;"""
        cur.execute(rider_query, (login_data.get('nickname'),))
        rider_obj = cur.fetchone()
        if not rider_obj:
            return make_response({'resp': 'no rider with such nickname'}, 400)

    if not (is_valid_password(rider_obj['password'], login_data['password'])):
        return make_response({'resp': 'invalid password'}, 400)

    user_id = rider_obj['id']

    access_token = encode_access_token(user_id)
    refresh_token = encode_refresh_token(user_id)

    if access_token and refresh_token:
        with db_connection.cursor() as cur:
            insert_token_query = """UPDATE rider SET refresh_token=%s, exp_time=%s WHERE id=%s;"""
            cur.execute(insert_token_query, (refresh_token, REFRESH_TOKEN_EXP_TIME(), user_id))

        response = {'uid': rider_obj['uuid'],
                    'refresh_token': refresh_token,
                    'access_token': access_token}
        return make_response({'resp': response}, 200)
    else:
        return make_response({'msg': 'try again'}, 500)
