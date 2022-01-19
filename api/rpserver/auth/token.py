from datetime import datetime

from flask import g, request, make_response
from psycopg2.extras import DictCursor
from werkzeug.exceptions import BadRequest
from jwt import InvalidTokenError

from . import auth_bp

from .auth_utils import (
    REFRESH_TOKEN_EXP_TIME, 
    encode_access_token, 
    encode_refresh_token, 
    decode_refresh_token,
)


@auth_bp.route('/new-token', methods=['GET'])
def gen_new_access_token():
    payload = decode_refresh_token(request.headers.get('refresh_token'))
    user_id = payload['user_id']
    connection = g.get('db_connection')
    with connection.cursor(cursor_factory=DictCursor) as cur:
        get_token_exp_time_query = """SELECT refresh_token, exp_time FROM rider WHERE id=%s;"""
        cur.execute(get_token_exp_time_query, (user_id,))
        result = cur.fetchone()
        if not result:
            raise BadRequest
        token_exp_time = result['exp_time']
        refresh_token = result['refresh_token'] 
        if datetime.utcnow() > token_exp_time or refresh_token == '':
            raise InvalidTokenError
        new_access_token = encode_access_token(user_id)
        new_refresh_token = encode_refresh_token(user_id)

        cur.execute("""UPDATE rider SET refresh_token=%s, exp_time=%s WHERE id=%s""", (new_refresh_token, REFRESH_TOKEN_EXP_TIME(), user_id))
    return make_response({'access_token': new_access_token,
                          'refresh_token': new_refresh_token}, 200)
