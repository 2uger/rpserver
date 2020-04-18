from flask_socketio import emit


from ridersPlatform import socketio, db
from ridersPlatform.models import Coordinate


@socketio.on('connect')
def client_connect():
    emit('connect', {'data': 'U r connected to websocket'})


@socketio.on('update_coordinates')
def rewrite_coordinates(updated_coordinates):
    coordinates = Coordinate.query.filter(Coordinate.rider_name == updated_coordinates['rider_name']).first()
    if coordinates is None:
        coordinates = Coordinate()
        Coordinate.add_to_db(coordinates.from_dict())
        emit('new_coordinates', coordinates)
    Coordinate.add_to_db(coordinates.from_dict())
    emit('new_coordinates', coordinates.to_dict())


@socketio.on('disconnect')
def client_disconnect():
    emit('disconnect', {'data': 'Goodbye'})
