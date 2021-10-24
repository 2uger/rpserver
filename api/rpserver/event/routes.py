from flask import request, make_response, jsonify, g
from werkzeug.exceptions import BadRequest

from rpserver.validation import PatchEventSchema, PostEventSchema

from . import event_bp


@event_bp.route('/', methods=['POST'])
def create_event():
    event_info = request.get_json()
    PostEventSchema().load(event_info)
    db_connection = g.get('db_connection')
    insert_event_query = """INSERT INTO ocassion(title, description, when_date, rider_id, spot_id) VALUES(%s, %s, %s, %s, %s)"""
    with db_connection.cursor() as cur:
        cur.execute(insert_event_query, (event_info['title'],
                                         event_info['description'],
                                         event_info['when_date'],
                                         event_info['rider_id'],
                                         event_info['spot_id']))
    return make_response({'resp': 'event has been created'}, 200)


@event_bp.route('/<int:event_id>', methods=['GET'])
def get_event(event_id):
    db_connection = g.get('db_connection')

    get_event_query = """SELECT * FROM ocassion WHERE id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(get_event_query, (event_id,))
        event_data = cur.fetchone() or {}
    resp = {name: value for name, value in event_data.items()}

    return make_response({'resp': resp}, 200)


@event_bp.route('/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    event_update_info = request.get_json()
    PatchEventSchema().load(event_update_info)

    db_connection = g.get('db_connection')
    update_event_query = """UPDATE ocassion SET title=%s, description=%s, when_date=%s, rider_id=%s, spot_id=%s WHERE id=%s"""
    with db_connection.cursor() as cur:
        get_event_query = """SELECT title, description, when_date, rider_id, spot_id, id FROM ocassion WHERE id=%s"""
        cur.execute(get_event_query, (event_id,))
        event_info = cur.fetchone()
        if not event_info:
            raise BadRequest

        cur.execute(update_event_query, [event_update_info.get(name) or event_info.get(name) for name,_ in event_info.items()])

    return make_response({'resp': 'event has been updated'}, 200)


@event_bp.route('/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    db_connection = g.get('db_connection')
    delete_event_query = """DELETE FROM ocassion WHERE id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(delete_event_query, (event_id,))
    return make_response({'resp': 'event has been deleted'}, 200)


@event_bp.route('/on-spot/<int:spot_id>', methods=['GET'])
def get_events_by_spot(spot_id):
    db_connection = g.get('db_connection')
    events_by_spot = """SELECT * FROM ocassion WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(events_by_spot, (spot_id,))
        events = cur.fetchall()

    resp = []
    for event in events:
        resp.append({name: value for name, value in event.items()})
    return make_response({"resp": resp}, 200)
