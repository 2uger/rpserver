from flask import request, make_response, g
from werkzeug.exceptions import BadRequest

from .schema import SpotSchema, UpdateSpotSchema

from rpserver.utils import (
    db_delete,
    db_select, 
    serialize, 
    serialize_many,
)
from .utils import spots_by_area
from . import spot_bp


@spot_bp.route('/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    db_connection = g.get('db_connection')
    
    spot = db_select('spot', db_connection, spot_id)
    if not spot:
        raise BadRequest

    return make_response({'resp': serialize(spot)}, 200)


@spot_bp.route('/', methods=['POST'])
def register_spot():
    spot_info = request.get_json()

    SpotSchema().load(spot_info)

    db_connection = g.get('db_connection')
    insert_spot_query = """INSERT INTO spot(title, coordinates, notes, profile_image_url) VALUES(%s, point(%s, %s), %s, %s)"""
    with db_connection.cursor() as cur:
        cur.execute(insert_spot_query, (spot_info.get('title'),
                                        spot_info.get('coordinates')[0],
                                        spot_info.get('coordinates')[1],
                                        spot_info.get('notes'),
                                        spot_info.get('profile_image_url')))
    return make_response({'resp': 'spot has been added'}, 200)


@spot_bp.route('/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    # Recieve all fields 
    spot_update_info = request.get_json()
    UpdateSpotSchema().load(spot_update_info)

    db_connection = g.get('db_connection')
    update_spot_query = """UPDATE spot SET title=%s, coordinates=point(%s, %s), notes=%s, profile_image_url=%s WHERE id=%s"""
    with db_connection.cursor() as cur:
        get_spot_query = """SELECT * FROM spot WHERE id=%s"""
        cur.execute(get_spot_query, (spot_id,))
        spot_info = cur.fetchone()
        if not spot_info:
            raise BadRequest("Oh, no such spot out here, maybe add new one!")
        cur.execute(update_spot_query, (spot_update_info.get('title') or spot_info['title'],
                                        spot_update_info.get('coordinates', (0.0,))[0] 
                                        or tuple(map(float, spot_info['coordinates'].strip('()').split(',')))[0],
                                        spot_update_info.get('coordinates', (0.0, 0.0))[1] 
                                        or tuple(map(float, spot_info['coordinates'].strip('()').split(',')))[1],
                                        spot_update_info.get('notes') or spot_info['notes'],
                                        spot_update_info.get('profile_image_url') or spot_info['profile_image_url'],
                                        spot_id))

    return make_response({'resp': 'spot has been updated'}, 200)


@spot_bp.route('/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    db_connection = g.get('db_connection')
    db_delete('spot', spot_id, db_connection)
    return make_response({'resp': 'spot has been deleted'}, 200)


@spot_bp.route('/by-area', methods=['GET'])
def get_spot_by_area():
    """Return list of spots inside certain area."""
    area_coordinates = request.get_json().get('area') or ((0.0, 0.0), (0.0, 0.0))
    db_connection = g.get('db_connection')
    spots = spots_by_area(db_connection, area_coordinates)
    if not spots:
        raise BadRequest
    return make_response({'resp': serialize_many(spots)}, 200)
