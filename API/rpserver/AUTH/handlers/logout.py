from datetime import datetime

from flask import request, g, make_response

from . import auth_bp
from .auth_utils import decode_refresh_token


@auth_bp.route('/logout', methods=['POST'])
def logout():
    """ Adding refresh_token in blacklist with logout_time """

    refresh_token = request.headers.get('refresh_token', None)
    if refresh_token:
        payload = decode_refresh_token(refresh_token)
        db_connection = g.get('db_connection')
        insert_token_query = """INSERT INTO token_blacklist(token_value, rider_id, expiration_time) VALUES(%s, %s, %s)"""
        with db_connection.cursor() as cur:
            cur.execute(insert_token_query, (refresh_token, rider_id, payload['exp']))
        make_response({'message': 'Logout success'}, 200)
    else:
        make_response({'message': 'Provide valiable refresh_token'}, 400)
