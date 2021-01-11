"""
API requests for spots
"""


from flask import request, make_response, g

from . import spot_bp


@spot_bp.route('/', methods=['POST'])
def register_spot():
    spot_data = request.get_json()

    # TODO: make data validation
    print(request)
    print(dir(request))

    db_connection = g.get('db_connection')
    insert_spot_query = """INSERT INTO spot(title, coordinates, notes) VALUES(%s, point(%s, %s), %s)"""
    with db_connection.cursor() as cur:
        cur.execute(insert_spot_query, (spot_data['title'],
                                        spot_data['coordinates'][0],
                                        spot_data['coordinates'][1],
                                        spot_data['notes']))
    return make_response({'message': 'Spot has been added'}, 200)


@spot_bp.route('/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    db_connection = g.get('db_connection')

    get_spot_query = """SELECT title, coordinates, notes FROM spot WHERE spot_id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_spot_query, (spot_id,))
        spot_data = cur.fetchone()

    return make_response({'msg': spot_data}, 200)


@spot_bp.route('/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    # Recieve all fields 
    spot_update_data = request.get_json()

    db_connection = g.get('db_connection')
    update_spot_query = """UPDATE spot SET title=%s WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_spot_query, (spot_update_data['title'], spot_id))

    return make_response({'msg': 'Spot has been updated'}, 200)


@spot_bp.route('/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    db_connection = g.get('db_connection')
    delete_spot_query = """DELETE FROM spot WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(delete_spot_query, (spot_id,))
    return make_response({'msg': 'Spot has been deleted'}, 200)


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
