from datetime import datetime
import hashlib

from flask import g, make_response, request

from . import auth_bp
from .auth_utils import hash_password, encode_access_token, encode_refresh_token


@auth_bp.route('/sign-up', methods=['POST'])
def rider_registration():
    rider_registration_data = request.get_json()

    # PostUserSchema().load(rider_registration_data)

    connection = g.get("db_connection")
    
    with connection.cursor() as cur:
        rider_insert_query = """INSERT INTO rider(nickname, email, password, hometown, registration_date)  
                                VALUES(%s, %s, %s, %s, %s);"""

        cur.execute(rider_insert_query, (rider_registration_data["nickname"],
                                         rider_registration_data["email"],
                                         hash_password(rider_registration_data["password"]),
                                         rider_registration_data["hometown"],
                                         datetime.today()))

    return make_response({"msg": "Registration completed"}, 200)


