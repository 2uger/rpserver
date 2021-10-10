from datetime import datetime, timedelta


from flask import make_response, request, g
from psycopg2 import ProgrammingError
from psycopg2.extras import DictCursor


from . import auth_bp
from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         is_valid_password)


REFRESH_TOKEN_EXP_TIME = datetime.now() + timedelta(days=20)
 

@auth_bp.route('/sign-in', methods=['POST'])
def rider_login():
    """ Rider login to send access_token and refresh_token back """

    login_data = request.get_json()

    # TODO: make user data check
    # LoginUserSchema().loads(login_data)

    db_connection = g.get("db_connection")
    
    # Check is email exist
    with db_connection.cursor(cursor_factory=DictCursor) as cur:
        rider_id_query = """SELECT * FROM rider WHERE email = %s;"""
        cur.execute(rider_id_query, (login_data.get("email"),))
        rider_information = cur.fetchone()
        if not rider_information:
            return make_response({"msg": "Wrong email"}, 400)

    if not (is_valid_password(login_data.get("password"), rider_information["password"])):
        return make_response({"msg": "Invalid password"}, 400)

    refresh_token = encode_refresh_token(rider_information["rider_id"])
    access_token = encode_access_token(rider_information["rider_id"])
    if access_token and refresh_token:
        with db_connection.cursor() as cur:
            insert_token_query = """UPDATE rider SET refresh_token=%s, token_exp_time=%s WHERE rider_id=%s;"""
            cur.execute(insert_token_query, (refresh_token, REFRESH_TOKEN_EXP_TIME, rider_information["rider_id"]))

        response = {"refresh_token": refresh_token,
                    "access_token": access_token}
        return make_response(response, 200)
    else:
        return make_response({"msg": "Try again"}, 500)

