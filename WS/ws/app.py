import asyncio

# import aioredis
from aiohttp import web

from routes import routes_post
from middleware import check_access_token, error_middleware
from config.settings import *


async def create_app():
    app = web.Application(client_max_size=MAX_REQUEST_SIZE, 
                          middlewares=[error_middleware])

    # For store user's friends activity(1)
    # and user coordinates(2)
    # redis_friends_activity = await aioredis.create_pool(REDIS_URI, db=1)
    # redis_coordinates = await aioredis.create_pool(REDIS_URI, db=2)
    # app.redis_p = redis_friends_activity
    # app.redis_pool_2 = redis_coordinates

    # Словарь с обектами сокетов и id пользователя
    app.connections = {}
    app.coordinates = {}
    
    app.add_routes([web.get(x[0], x[1]) for x in routes_post])

    # Close:
    # client_session
    # web_sockets
    # connection to redis
    app.on_cleanup.append(on_cleanup)

    return app


async def on_cleanup(app):
    # closing redis pool and connection
    # app.redis.close()
    # await app.wait_closed()

    for ws in app.connections.values():
        if not ws.closed():
            await ws.close()
