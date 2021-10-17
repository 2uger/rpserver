from datetime import datetime
import hashlib

from flask import g, make_response, request

from .auth_utils import hash_password, encode_access_token, encode_refresh_token

from . import auth_bp


@auth_bp.route('/sign-up', methods=['POST'])
def rider_registration():
    # generate sequence
    close_key = 2
    
    connection = g.get('db_connection')
    with connection.cursor() as cur:
        rider_insert_query = """INSERT INTO rider(close_key) VALUES(%s);"""
        cur.execute(rider_insert_query, (hash_seq(close_key),))

    return make_response({'resp': close_key}, 200)


