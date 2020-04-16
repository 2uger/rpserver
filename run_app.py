from flask import current_app
from flask_cors import CORS

from ridersPlatform import create_app, socketio


app = create_app()
CORS(app)
if __name__ == '__main__':
    socketio.run(app, port=5002)
