"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, current_app

from . import spot_bp

from rpserver.api.handlers import SpotHandler
from rpserver.api.utils import JSONResponse


@spot_bp.route('/add/', methods=['POST'])
def register_spot():
    spot_data = request.get_json()
    try:
        PostspotSchema().load(object_data)
    except ValidationError as ve:
        #loggin
        make_response('ValidationError', 400)
    query = insert([spot_table]).values(spot_data)
    with engine.connect() as connection:
            result = connection.execute(query).fetchall()
    make_response(, 200)


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    if spot_id < 0:
        make_response(, 400)
    query = select([spot_table]).where(spot_table.c.spot_id == spot_id)
    with engine.connect() as connection:
        result = connection.execute(query).fetchall()
    make_response(result, 200)


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    update_spot_data = request.get_json()
    try:
        PatchspotSchema().load(update_spot_data)
    except ValidationError as:
        #logging
        make_response('ValidationError', 400)
    query = update([spot_table]).where(spot_table.c.spot_id == spot_id).values(update_spot_data)
    with engine.connect() as connection:
        response = connection.execute(query)
    make_response(response, 200)


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    if spot_id < 0:
        make_response('Invalid spot ID', 400)
    query = delete([spot_table]).where(spot_table.c.spot_id == spot_id)
    with engine.connect() as connection:
        connection.execute(query)
    make_response(, 200)
