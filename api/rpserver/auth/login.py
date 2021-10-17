from datetime import datetime, timedelta

from flask import make_response, request, g
from psycopg2 import ProgrammingError
from psycopg2.extras import DictCursor

from .auth_utils import (encode_access_token, 
                         encode_refresh_token,
                         is_valid_close_key)

from . import auth_bp


REFRESH_TOKEN_EXP_TIME = datetime.now() + timedelta(days=20)


@auth_bp.route('/sign-in/<string:rider_uuid>', methods=['POST'])
def rider_login(rider_uuid: str):
    """Create refresh and access tokens for rider."""

    login_data = request.get_json()

    # TODO: make user data check

    db_connection = g.get('db_connection')
    
    with db_connection.cursor(cursor_factory=DictCursor) as cur:
        rider_query = """SELECT * FROM rider WHERE uuid = %s;"""
        cur.execute(rider_query, (rider_uuid,))
        rider_information = cur.fetchone()
        if not rider_information:
            return make_response({'resp': 'wrong uuid'}, 400)

    if not (is_valid_close_key(login_data.get('hash_close_key'), rider_information['close_key'])):
        return make_response({'resp': 'invalid close key'}, 400)

    access_token = encode_access_token(rider_uuid)
    refresh_token = encode_refresh_token(rider_uuid)
    if access_token and refresh_token:
        with db_connection.cursor() as cur:
            insert_token_query = """UPDATE rider SET refresh_token=%s, token_exp_time=%s WHERE id=%s;"""
            cur.execute(insert_token_query, (refresh_token, REFRESH_TOKEN_EXP_TIME, rider_information['id']))

        response = {"refresh_token": refresh_token,
                    "access_token": access_token}
        return make_response(response, 200)
    else:
        return make_response({"msg": "Try again"}, 500)
