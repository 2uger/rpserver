"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import jsonify, request, make_response, g
from sqlalchemy import insert, select, delete, update

from . import spot_bp

from rpserver.db.schema import spot_table
from rpserver.api.app import db_connection

from ..deserialize import deserialize_to_dict
from ..valid_data_schema import PostSpotSchema 


@spot_bp.route('/add/', methods=['POST'])
def register_spot():
    spot_data = request.get_json()
    PostSpotSchema().load(spot_data)
    insert_spot_query = spot_table.insert().values(spot_data)
    connection = g.db_connection
    result = connection.execute(insert_spot_query)
    return make_response({"message": "Spot has been added"}, 200)


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    if spot_id < 0:
        return make_response({'error': {'message': 'Spot id can not be less than 0'}}, 400)
    get_spot_query = spot_table.select().where(spot_table.c.spot_id == spot_id)
    connection = g.db_connection
    result = connection.execute(get_spot_query).fetchall()
    return make_response({'message': result}, 200)


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    update_spot_data = request.get_json()
   
    update_spot_query = spot_table.update().where(spot_table.c.spot_id == spot_id).values(update_spot_data)
    connection = g.db_connection
    response = connection.execute(update_spot_query)
    return make_response({"message": "Spot has been updated"}, 200)


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    if spot_id < 0:
        return make_response('Invalid spot ID', 400)
    delete_spot_query = delete([spot_table]).where(spot_table.c.spot_id == spot_id)
    connection = g.db_connection
    connection.execute(delete_spot_query)
    return make_response({"message": "Deleted"}, 200)
