"""
API requests for riders
"""


from flask import request, make_response, jsonify, g

from . import rider_bp


@rider_bp.route('/get/<int:rider_id>', methods=['GET'])
def get_rider(rider_id):
    db_connection = g.get('db_connection')

    get_rider_query = """SELECT nickname, bio, hometown, registration_date FROM rider WHERE rider_id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_rider_query, (rider_id,))
        rider_data = cur.fetchone()

    return make_response({'msg': rider_data}, 200)


@rider_bp.route('/update/<int:rider_id>', methods=['PATCH'])
def update_rider(rider_id):
    rider_update_data = request.get_json()

    db_connection = g.get('db_connection')
    update_rider_query = """UPDATE rider SET nickname=%s, bio=%s, hometown=%s WHERE rider_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_rider_query, (rider_update_data['nickname'], 
                                         rider_update_data.get('bio', None),
                                         rider_update_data.get('hometown', None),
                                         rider_id))

    return make_response({'msg': 'Rider has been updated'}, 200)


@rider_bp.route('/friends/<int:rider_id>', methods=['GET'])
def get_friends(rider_id):
    pass


@rider_bp.route('/friendship/send/<int:rider_id>', methods=['POST'])
def send_friendship(rider_id):
    pass


@rider_bp.route('/friendship/accept/<int:rider_id>', methods=['POST'])
def accept_friendship(rider_id):
    pass


@rider_bp.route('/friendship/remove/<int:rider_id>', methods=['POST'])
def remove_friendship(rider_id):
    pass




