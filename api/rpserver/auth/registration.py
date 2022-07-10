from uuid import uuid4

from flask import current_app, g, make_response, request

from rpserver.rider.schema import PostUserSchema
from .auth_utils import hash_password

from . import auth_bp


@auth_bp.route('/sign-up', methods=['POST'])
def rider_registration():
    user_info = request.get_json()

    current_app.logger.info(f'Incoming request: {request}')
    PostUserSchema().load(user_info)
    connection = g.get('db_connection')
    
    with connection.cursor() as cur:
        rider_insert_query = """INSERT INTO rider(nickname, password, uuid)  
                                VALUES(%s, %s, %s)"""
        user_uuid = str(uuid4())
        cur.execute(rider_insert_query, (user_info['nickname'],
                                         hash_password(user_info['password']),
                                         user_uuid))
        rider_select = """SELECT id FROM rider WHERE nickname=%s;"""
        cur.execute(rider_select, (user_info['nickname'],))
        rider_id = cur.fetchone().get('id')

    return make_response({'resp': {'id': rider_id,
                                   'uuid': user_uuid}}, 200)

