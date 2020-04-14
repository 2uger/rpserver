from flask import current_app


from ridersPlatform import create_app, socketio


app = create_app()
app.logger
if __name__ == '__main__':
    socketio.run(app)
