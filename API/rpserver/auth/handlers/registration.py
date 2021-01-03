from datetime import datetime
import hashlib

from flask import g, make_response, request

from . import auth_bp
from .auth_utils import encode_access_token, encode_refresh_token


@auth_bp.route('/registration', methods=['POST'])
def rider_registration():
    rider_registration_data = request.get_json()

    # PostUserSchema().load(rider_registration_data)

    connection = g.get("db_connection")
    
    # Check if there is user with the same email

    with connection.cursor() as cur:
        cur.execute("SELECT email FROM rider WHERE email = %s;", (rider_registration_data.get("email"),))
        result = cur.fetchall()
        if result:
            return make_response({"msg": "Same email exist"}, 404)

        rider_insert_query = """INSERT INTO rider(nickname, email, password, hometown, registration_date)  
                                VALUES(%s, %s, %s, %s, %s);"""

        password_hash = hashlib.sha256()
        password_hash.update(rider_registration_data["password"].encode())

        cur.execute(rider_insert_query, (rider_registration_data["nickname"],
                                         rider_registration_data["email"],
                                         password_hash.hexdigest(),
                                         rider_registration_data["hometown"],
                                         datetime.today()))

    return make_response({"msg": "Registration completed"}, 200)


