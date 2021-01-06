"""
API requests for event on map
"""


from flask import request, make_response, jsonify, g


from . import event_bp


@event_bp.route('/add', methods=['POST'])
def add_event():
    event_data = request.get_json()
    db_connection = g.get('db_connection')
    insert_event_query = """INSERT INTO ocassion(nickname, description, when_date, rider_id, spot_id) VALUES(%s, %s, %s, %s, %s)"""
    with db_connection.cursor() as cur:
        cur.execute(insert_event_query, (event_data['nickname'],
                                         event_data['description'],
                                         event_data['date'],
                                         event_data['rider_id'],
                                         event_data['spot_id']))
    return make_response({'msg': 'Event has been created'}, 200)


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    db_connection = g.get('db_connection')

    get_event_query = """SELECT * FROM ocassion WHERE ocassion_id = %s"""
    with db_connection.cursor() as cur:
        cur.execute(get_event_query, (event_id,))
        event_data = cur.fetchone()

    return make_response({'event info': event_data}, 200)


@event_bp.route('/update/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    event_update_data = request.get_json()

    db_connection = g.get('db_connection')
    update_event_query = """UPDATE ocassion SET nickname=%s WHERE ocassion_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_event_query, (event_update_data['nickname'], event_id))

    return make_response({'msg': 'Event has been updated'}, 200)


@event_bp.route('/delete/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    db_connection = g.get('db_connection')
    delete_event_query = """DELETE FROM ocassion WHERE ocassion_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(delete_event_query, (event_id,))
    return make_response({'msg': 'Event has been deleted'}, 200)
