"""
API requests for event on map
"""


from flask import request, make_response, jsonify
from sqlalchemy import select, insert, update, delete

from rpserver.db.schema import event_table

from ..valid_data_schema import PostEventSchema, PatchEventSchema
from . import event_bp


@event_bp.route('/add/', methods=['POST'])
def add_event():
    event_data = request.get_json()
    PostEventSchema().load(event_data)
    #loggin
    add_event_query = insert([event_table]).values(event_data)
    connection = db_connection()
    result = connection.execute(add_event_query).fetchall()
    make_response({'message': 'Event have been created'}, 200)


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    if event_id < 0:
        make_response({'message': 'Invalid event_id'}, 400)
    get_event_query = select([event_table]).where(event_table.c.event_id == event_id)
    connection = db_connection()
    result = connection.execute(get_event_query).fetchall()

    make_response(jsonify(result), 200)


@event_bp.route('/update/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    update_event_data = request.get_json()
    PatchEventSchema().load(update_event_data)

    #logging

    update_event_query = update([event_table]).where(event_table.c.event_id == event_id).values(update_event_data)
    connection = db_connection()
    result = connection.execute(update_event_query)

    make_response(jsonify(result), 200)


@event_bp.route('/delete/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    if event_id < 0:
        make_response('Invalid event ID', 400)
    delete_event_query = delete([event_table]).where(event_table.c.event_id == event_id)
    connection = db_connection()
    result = connection.execute(delete_event_query)

    make_response(jsonify(result), 200)
