from datetime import datetime
from flask import g, make_response, request


from . import auth_bp
from .auth_utils import encode_access_token, encode_refresh_token


@auth_bp.route('/add', methods=['POST'])
def rider_registration():
    rider_registration_data = request.get_json()

    # PostUserSchema().load(rider_registration_data)

    connection = g.get("db_connection")
    
    # Check if there is user with the same email
    try:
        with connection.cursor() as cur:
            cur.execute("SELECT email FROM rider WHERE email = %s;", (rider_registration_data.get("email"),))
            result = cur.fetchall()
            if result:
                return make_response({"msg": "Wrong email"}, 404)

            rider_insert_query = """INSERT INTO rider(nickname, email, passwrd, hometown, registration_date)  
                                VALUES(%s, %s, %s, %s, %s);"""
            cur.execute(rider_insert_query, (rider_registration_data["nickname"],
                                            rider_registration_data["email"],
                                            rider_registration_data["password"],
                                            rider_registration_data["hometown"],
                                            datetime.today()))
    except Exception as e:
        return make_response({"msg": e})

    return make_response({"msg": "Registration completed"}, 200)


