"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, g

from . import spot_bp

from rpserver.db.schema import spot_table
from ..valid_data_schema import PostSpotSchema, PatchSpotSchema


@spot_bp.route('/add', methods=['POST'])
def register_spot():
    spot_data = request.get_json()

    # TODO: make data validation
    db_connection = g.get('db_connection')
    
    insert_spot_query = """"INSERT INTO spot(nickname, coordinates) VALUES(%s, point(%s, %s))""""
    with db_connection.cursor() as cur:
        cur.execute(insert_spot_query, (spot_data['nickname'],
                                        coordinates[0],
                                        coordinates[1]))
    return make_response({'message: Spot has been added'}, 200)


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    db_connection = g.get('db_connection')

    get_spot_query = """SELECT nickname, coordinates, notes FROM spot WHERE spot_id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_spot_query, (spot_id,))
        spot_information = cur.fetchone()

    return make_response({'spot info': result}, 200)


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    return make_response({'update spot': 'update spot'}, 200)


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    return make_response({'delete spot': 'delete spot'}, 200)
