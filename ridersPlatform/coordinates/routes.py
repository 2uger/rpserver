from flask_socketio import emit


from ridersPlatform import socketio, db
from ridersPlatform.models import Coordinates


@socketio.on('connect')
def client_connect():
    emit('connect', {'data': 'U r connected to websocket'})


@socketio.on('update_coordinates')
def rewrite_coordinates(updated_coordinates):
    coordinates = Coordinates.query.filter(Coordinates.rider_id == updated_coordinates['rider_id']).first()
    if coordinates is None:
        coordinates = Coordinates()
        Coordinates.add_to_db(coordinates.from_dict())
        emit('new_coordinates', coordinates)
    Coordinates.add_to_db(coordinates.from_dict())
    emit('new_coordinates', coordinates.to_dict())


@socketio.on('disconnect')
def client_disconnect():
    emit('disconnect', {'data': 'Goodbye'})
