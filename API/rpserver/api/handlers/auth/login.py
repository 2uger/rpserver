from flask import make_response, request, g
from psycopg2 import ProgrammingError


from . import auth_bp
from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         is_valid_password)



@auth_bp.route('/login', methods=['POST'])
def rider_login():
    """ Rider login to send access_token and refresh_token back """

    login_data = request.get_json()

    # TODO: make user data check
    # LoginUserSchema().loads(login_data)

    db_connection = g.get("db_connection")
    
    with db_connection.cursor() as cur:
        rider_id_query = """SELECT * FROM rider WHERE email = %s;"""
        try:
            cur.execute(rider_id_query, (login_data.get("email"),))
            rider_information = cur.fetchone()
        except ProgrammingError as e:
            return make_response({"msg": "Wrong email"}, 400)

    #if not (is_valid_password(login_data.get("password"), rider_information[2])):
    #    return make_response({"msg": "Invalid password"}, 400)


    refresh_token = encode_access_token(rider_information[2])
    access_token = encode_access_token(rider_information[2])
    print(refresh_token)
    if access_token and refresh_token:
        response = {"refresh_token": refresh_token.decode(),
                    "auth_token": access_token.decode()}
        return make_response(response, 200)
    else:
        return make_response({"msg": "Try again"}, 500)

