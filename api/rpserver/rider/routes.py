from flask import request, make_response, jsonify, g
from werkzeug.exceptions import BadRequest

from rpserver.utils import db_select, db_update, serialize, serialize_many
from .schema import PatchUserSchema
from . import rider_bp


@rider_bp.route('/<int:rider_id>', methods=['GET'])
def get_rider(rider_id):
    db_connection = g.get('db_connection')

    rider = db_select('rider', db_connection, rider_id)
    if not rider:
        raise BadRequest

    return make_response({'resp': serialize(rider)}, 200)


@rider_bp.route('/', methods=['GET'])
def get_rider_list():
    db_connection = g.get('db_connection')
    
    riders = db_select('rider', db_connection, many=True)
    if not riders:
        raise BadRequest

    return make_response({'resp': serialize_many(riders)}, 200)


@rider_bp.route('/', methods=['PATCH'])
def update_rider():
    rider_update_data = request.get_json()
    PatchUserSchema().load(rider_update_data)

    db_connection = g.get('db_connection')

    db_update(rider_update_data, 'rider', request.user_id, db_connection)

    return make_response({'resp': 'rider has been updated'}, 200)

