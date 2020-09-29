"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, g
from sqlalchemy import insert, select, delete, update

from . import spot_bp

from rpserver.db.schema import spot_table
from ..valid_data_schema import PostSpotSchema, PatchSpotSchema


@spot_bp.route('/add/', methods=['POST'])
def register_spot():
    spot_data = request.get_json()
    PostSpotSchema().load(spot_data)
    insert_spot_query = insert([spot_table]).values(spot_data)
    connection = g.get('database')
    result = connection.execute(insert_spot_query).fetchall()
    make_response({'message: Spot has been added'}, 200)


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    if spot_id < 0:
        make_response({'error': {'message': 'Spot id can not be less than 0'}}, 400)
    get_spot_query = select([spot_table]).where(spot_table.c.spot_id == spot_id)
    connection = g.get('database')
    result = connection.execute(get_spot_query).fetchall()
    make_response(result, 200)


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    update_spot_data = request.get_json()
    PatchSpotSchema().load(update_spot_data)
    update_spot_query = update([spot_table]).where(spot_table.c.spot_id == spot_id).values(update_spot_data)
    connection = g.get('database')
    response = connection.execute(update_spot_query)
    make_response(response, 200)


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    if spot_id < 0:
        make_response('Invalid spot ID', 400)
    delete_spot_query = delete([spot_table]).where(spot_table.c.spot_id == spot_id)
    connection = g.get('database')
    connection.execute(delete_spot_query)
    make_response({'message': 'Spot has been added'}, 200)
