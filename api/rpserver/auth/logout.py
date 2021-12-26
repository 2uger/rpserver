from datetime import datetime

from flask import request, g, make_response

from .auth_utils import decode_refresh_token

from . import auth_bp


@auth_bp.route('/revoke', methods=['POST'])
def logout():
    """Make refresh token get expired right now."""
    refresh_token = request.headers.get("refresh-token", None)
    if refresh_token:
        payload = decode_refresh_token(refresh_token)
        db_connection = g.get('db_connection')
        insert_token_query = """UPDATE rider SET refresh_token=%s, exp_time=%s WHERE id=%s"""
        with db_connection.cursor() as cur:
            cur.execute(insert_token_query, ('', datetime.utcnow(), payload['user_id']))
        return make_response({'msg': 'logout success'}, 200)
    else:
        return make_response({'msg': 'provide valiable refresh_token'}, 400)

