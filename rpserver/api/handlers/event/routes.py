"""
API requests for event on map
"""


from flask import request, make_response

from ..validation_schema import 

from . import event_bp



@event_bp.route('/add/', methods=['POST'])
def add_event():
    event_data = request.get_json()
    try:
        PosteventSchema().load(object_data)
    except ValidationError as ve:
        #loggin
        make_response('ValidationError', 400)
    query = insert([event_table]).values(event_data)
    with engine.connect() as connection:
            result = connection.execute(query).fetchall()
    make_response(, 200)


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    if event_id < 0:
        make_response(, 400)
    query = select([event_table]).where(event_table.c.event_id == event_id)
    with engine.connect() as connection:
        result = connection.execute(query).fetchall()
    make_response(result, 200)


@event_bp.route('/update/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    update_event_data = request.get_json()
    try:
        PatcheventSchema().load(update_event_data)
    except ValidationError as:
        #logging
        make_response('ValidationError', 400)
    query = update([event_table]).where(event_table.c.event_id == event_id).values(update_event_data)
    with engine.connect() as connection:
        response = connection.execute(query)
    make_response(response, 200)


@event_bp.route('/delete/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    if event_id < 0:
        make_response('Invalid event ID', 400)
    query = delete([event_table]).where(event_table.c.event_id == event_id)
    with engine.connect() as connection:
        connection.execute(query)
    make_response(, 200)
