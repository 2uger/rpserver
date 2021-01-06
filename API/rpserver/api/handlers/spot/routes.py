"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, g

from . import spot_bp


@spot_bp.route('/add', methods=['POST'])
def register_spot():
    spot_data = request.get_json()

    # TODO: make data validation

    db_connection = g.get('db_connection')
    insert_spot_query = """INSERT INTO spot(nickname, coordinates, notes) VALUES(%s, point(%s, %s), %s)"""
    with db_connection.cursor() as cur:
        cur.execute(insert_spot_query, (spot_data['nickname'],
                                        spot_data['coordinates'][0],
                                        spot_data['coordinates'][1],
                                        spot_data['notes']))
    return make_response({'message': 'Spot has been added'}, 200)


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    db_connection = g.get('db_connection')

    get_spot_query = """SELECT nickname, coordinates, notes FROM spot WHERE spot_id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_spot_query, (spot_id,))
        spot_data = cur.fetchone()

    return make_response({'msg': spot_data}, 200)


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    spot_update_data = request.get_json()

    db_connection = g.get('db_connection')
    update_spot_query = """UPDATE spot SET nickname=%s WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_spot_query, (spot_update_data['nickname'], spot_id))

    return make_response({'msg': 'Spot has been updated'}, 200)


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    db_connection = g.get('db_connection')
    delete_spot_query = """DELETE FROM spot WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(delete_spot_query, (spot_id,))
    return make_response({'msg': 'Spot has been deleted'}, 200)

@spot_bp.route('/area', methods=['GET'])
def get_spot_in_area():
    # Get coordinates of spot in exact area

