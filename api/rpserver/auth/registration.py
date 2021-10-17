from datetime import datetime
import hashlib
import uuid

from flask import g, make_response, request

from .auth_utils import hash_sequence

from . import auth_bp


@auth_bp.route('/sign-up', methods=['POST'])
def rider_registration():
    rider_registration_info = request.get_json()

    connection = g.get("db_connection")
    
    with connection.cursor() as cur:
        rider_insert_query = """INSERT INTO rider(uuid, nickname, password, registration_date)  
                                VALUES(%s, %s, %s, %s);"""

        rider_uuid = str(uuid.uuid1())
        cur.execute(rider_insert_query, (rider_uuid,
                                         rider_registration_info['nickname'],
                                         hash_sequence(rider_registration_info['password']),
                                         datetime.today()))

    return make_response({'resp': rider_uuid}, 200)
