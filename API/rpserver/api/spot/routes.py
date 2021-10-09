from flask import request, make_response, g

from rpserver.api.validation import PostSpotSchema
from . import spot_bp


@spot_bp.route('/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    db_connection = g.get('db_connection')

    get_spot_query = """SELECT id, title FROM spot WHERE id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_spot_query, (spot_id,))
        spot_info = cur.fetchone()

    resp = {}
    if spot_info:
        resp = {'id': spot_info['id'],
                'title': spot_info['title'],}
    return make_response({'resp': resp}, 200)


@spot_bp.route('/', methods=['POST'])
def register_spot():
    spot_info = request.get_json()

    PostSpotSchema().load(spot_info)
    db_connection = g.get('db_connection')
    insert_spot_query = """INSERT INTO spot(title, coordinates) VALUES(%s, point(%s, %s))"""
    with db_connection.cursor() as cur:
        cur.execute(insert_spot_query, (spot_info.get('title', ''),
                                        spot_info.get('coordinates')[0],
                                        spot_info.get('coordinates')[1]))
    return make_response({'resp': 'spot has been added'}, 200)


@spot_bp.route('/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    # Recieve all fields 
    spot_update_data = request.get_json()

    db_connection = g.get('db_connection')
    update_spot_query = """UPDATE spot SET title=%s WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_spot_query, (spot_update_data['title'], spot_id))

    return make_response({'resp': 'Spot has been updated'}, 200)


@spot_bp.route('/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    db_connection = g.get('db_connection')
    delete_spot_query = """DELETE FROM spot WHERE id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(delete_spot_query, (spot_id,))
    return make_response({'resp': 'Spot has been deleted'}, 200)


@spot_bp.route('/by-area', methods=['GET'])
def get_spot_by_area():
    # Rectangle area
    area_coordinates = request.get_json()["area"]
    # List with x and y
    top_left_point = area_coordinates[0]
    bottom_right_point = area_coordinates[1]
    
    get_spot_by_area_query = """SELECT title, coordinates, notes FROM SPOT WHERE coordinates[0]>%s
                                                                           AND coordinates[0]<%s
                                                                           AND coordinates[1]>%s
                                                                           AND coordinates[1]<%s"""
    with db_connection.cursor() as cur:
        cur.execute(get_spot_by_area_query, (top_left_point[0], bottom_right_point[0],
                                             top_left_point[1], bottom_right_point[1]))
        spots_by_area = cur.fetchall()
    return make_response({"spots": spots_by_area}, 200)
