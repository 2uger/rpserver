from datetime import datetime, timedelta

from flask import make_response, request, g
from psycopg2 import ProgrammingError
from psycopg2.extras import DictCursor

from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         is_valid_password)

from . import auth_bp


REFRESH_TOKEN_EXP_TIME = datetime.now() + timedelta(days=20)
 

@auth_bp.route('/close-key<rider_id:int>', methods=['GET'])
def rider_login(rider_id: int):
    """Generate session key for user authentication."""

    random_key = 3
    return make_response({'resp': random_key}, 200)

@auth_bp.route('/session-key', methods=['POST'])
    user_side_session_key = request.get_json()
    connection = g.get('db_connection')
    
    # Check is email exist
    with connection.cursor(cursor_factory=DictCursor) as cur:
        close_key_query = """SELECT close_key FROM rider WHERE id=%s;"""
        cur.execute(close_key_query, (rider_id),)
        close_key = cur.fetchone()
        random_key = 2
        if not close_key or not random_key:
            return make_response({'resp': 'wrong rider id'}, 400)

    session_key = hash_by_seq(close_key + random_key)

    if session_key
