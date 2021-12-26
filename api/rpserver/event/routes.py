from flask import request, make_response, g
from werkzeug.exceptions import BadRequest

from rpserver.utils import (
    db_delete,
    db_insert, 
    db_select, 
    db_update, 
    serialize, 
    serialize_many,
)
from .schema import PatchEventSchema, PostEventSchema
from .utils import events_by_spot
from . import event_bp


@event_bp.route('/', methods=['POST'])
def create_event():
    event_info = request.get_json()
    PostEventSchema().load(event_info)
    db_connection = g.get('db_connection')

    db_insert(event_info, 'ocassion', db_connection)

    return make_response({'resp': 'event has been created'}, 200)


@event_bp.route('/<int:event_id>', methods=['GET'])
def get_event(event_id):
    db_connection = g.get('db_connection')

    event = db_select('ocassion', db_connection, event_id)
    if not event:
        raise BadRequest

    return make_response({'resp': serialize(event)}, 200)


@event_bp.route('/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    event_update_info = request.get_json()
    PatchEventSchema().load(event_update_info)

    db_connection = g.get('db_connection')

    db_update(event_update_info, 'ocassion', event_id, db_connection)

    return make_response({'resp': 'event has been updated'}, 200)


@event_bp.route('/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    db_connection = g.get('db_connection')
    db_delete('ocassion', event_id, db_connection)
    return make_response({'resp': 'event has been deleted'}, 200)


@event_bp.route('/on-spot/<int:spot_id>', methods=['GET'])
def get_events_by_spot(spot_id):
    db_connection = g.get('db_connection')
    events = events_by_spot(spot_id, db_connection)
    if not events:
        raise BadRequest
    return make_response({'resp': serialize_many(events)}, 200)
