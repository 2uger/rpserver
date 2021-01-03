from datetime import datetime

from flask import request, g, make_response

from . import auth_bp
from .auth_utils import decode_refresh_token


@auth_bp.route('/logout', methods=['POST'])
def logout():
    """ Adding refresh_token in blacklist with logout_time """

    refresh_token = request.headers.get('refresh_token', None)
    print(refresh_token)
    if refresh_token:
        payload = decode_refresh_token(refresh_token)
        print(payload)
        db_connection = g.get('db_connection')
        insert_token_query = """INSERT INTO blacklist_token(token_value, rider_id, expiration_time) VALUES(%s, %s, %s)"""
        with db_connection.cursor() as cur:
            cur.execute(insert_token_query, ('fdsjf', payload['sub'], datetime.fromtimestamp(payload['exp'])))
        return make_response({'message': 'Logout success'}, 200)
    else:
        return make_response({'message': 'Provide valiable refresh_token'}, 400)
