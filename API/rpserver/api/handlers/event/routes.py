"""
API requests for event on map
"""


from flask import request, make_response, jsonify, g
from sqlalchemy import select, insert, update, delete

from rpserver.db.schema import event_table

from ..valid_data_schema import PostEventSchema 
from . import event_bp


@event_bp.route('/add/', methods=['POST'])
def add_event():
    event_data = request.get_json()
    PostEventSchema().load(event_data)
    add_event_query = event_table.insert().values(event_data)
    connection = g.db_connection
    connection.execute(add_event_query)
    return make_response({'message': "All good"}, 200)


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    if event_id < 0:
        return make_response({'message': 'Invalid event_id'}, 400)
    get_event_query = event_table.select().where(event_table.c.event_id == event_id)
    connection = g.db_connection
    result = connection.execute(get_event_query).fetchall()

    return make_response({"message": result}, 200)


@event_bp.route('/update/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    update_event_data = request.get_json()
    
    update_event_query = event_table.update().where(event_table.c.event_id == event_id).values(update_event_data)
    connection = g.db_connection
    connection.execute(update_event_query)

    return make_response({"message": "Updated"}, 200)


@event_bp.route('/delete/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    if event_id < 0:
        return make_response({'error': 'Invalid event ID'}, 400)
    delete_event_query = event_table.delete().where(event_table.c.event_id == event_id)
    connection = g.db_connection
    result = connection.execute(delete_event_query)

    return make_response({"message": "Deleted"}, 200)
