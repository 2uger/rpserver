from datetime import datetime, timedelta

from flask import g, request, make_response
from psycopg2.extras import DictCursor
from jwt import InvalidTokenError


from . import auth_bp

from .auth_utils import encode_access_token, decode_refresh_token


@auth_bp.route("/new-token", methods=["GET"])
def gen_new_access_token():
    payload = decode_refresh_token(request.headers.get("refresh_token"))
    rider_id = payload["sub"]
    db_connection = g.get("db_connection")
    with db_connection.cursor(cursor_factory=DictCursor) as cur:
        get_token_exp_time_query = """SELECT refresh_token, token_exp_time FROM rider WHERE rider_id=%s;"""
        cur.execute(get_token_exp_time_query, (rider_id,))
        result = cur.fetchone()
        if not result:
            raise Exception
        token_exp_time = result["token_exp_time"]
        refresh_token = result["refresh_token"] 
        if datetime.utcnow() - token_exp_time > timedelta(seconds=0) or refresh_token == "":
            raise InvalidTokenError
    new_access_token = encode_access_token(rider_id)
    return make_response({"access_token": new_access_token}, 200)
