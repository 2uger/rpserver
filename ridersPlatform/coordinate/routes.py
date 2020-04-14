from flask import current_app
from flask_socketio import emit


from ridersPlatform import socketio, db
from ridersPlatform.models import Coordinate


@socketio.on('connected')
def client_connect(message):
    print(f'Client with {message} connected')


@socketio.on('update_coordinates', namespace='/coordinates')
def rewrite_coordinates(updated_coordinates):
    coordinates = Coordinate.query.filter_by(updated_coordinates['rider_id']).first()
    coordinates.coordinates = updated_coordinates['coordinates']
    db.session.add(coordinates)
    db.session.commit()
    emit('new_coordinates', coordinates.to_dict())


@socketio.on('disconnected')
def client_disconnect(message):
    print('Client disconnect')