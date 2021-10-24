from flask import request, make_response, jsonify, g

from . import rider_bp


@rider_bp.route('/<int:rider_id>/m', methods=['GET'])
def get_rider(rider_id):
    db_connection = g.get('db_connection')

    get_rider_query = """SELECT * FROM rider WHERE id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_rider_query, (rider_id,))
        rider_info = cur.fetchone()
    resp = {'id': rider_info['id'],
            'nickname': rider_info['nickname'],
            'hometown': rider_info['hometown']}

    return make_response({'resp': resp}, 200)


@rider_bp.route('/<int:rider_id>', methods=['PATCH'])
def update_rider(rider_id):
    rider_update_data = request.get_json()

    db_connection = g.get('db_connection')
    update_rider_query = """UPDATE rider SET nickname=%s, bio=%s, hometown=%s WHERE rider_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_rider_query, (rider_update_data['nickname'], 
                                         rider_update_data.get('bio', None),
                                         rider_update_data.get('hometown', None),
                                         rider_id))

    return make_response({'resp': 'Rider has been updated'}, 200)
