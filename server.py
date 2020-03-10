import socketio
import argparse
from aiohttp import web


PARSER = argparse.ArgumentParser()
PARSER.add_argument("host",
                    type=str,
                    nargs="?",
                    help="HOST",
                    default="127.0.0.1")

PARSER.add_argument("port",  
                    type=int,
                    nargs="?",
                    help="PORT, default=5000",
                    default=5000)

SIO = socketio.AsyncServer(async_mode="aiohttp", cors_allowed_origins="*")
APP = web.Application()
SIO.attach(APP)

USERNAMES = {}


@SIO.event
async def connect(sid, environ):
    print("Connect ", sid)

@SIO.event
async def disconnect(sid):
    global USERNAMES
    await SIO.emit("user_disconnect", {"user": USERNAMES.get(sid)}, skip_sid=sid)
    USERNAMES.pop(sid, None)
    print("Disconnect ", sid)

@SIO.event
async def message(sid, data):
    print(data)
    await SIO.emit("message", {"username": USERNAMES.get(sid), "message": data}, skip_sid=sid)

if __name__ == "__main__":
    args = PARSER.parse_args()
    web.run_app(APP, host=args.host, port=args.port)
