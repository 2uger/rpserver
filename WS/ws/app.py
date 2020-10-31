import asyncio

import aioredis
from aiohttp import web

from ws.routes import routes_post
from ws.middleware import check_access_token, error_middleware
from ws.config.settings import *


async def create_app():
    app = web.Application(client_max_size=MAX_REQUEST_SIZE, 
                          middlewares=[error_middleware, check_access_token])

    # For store user's friends activity(1)
    # and user coordinates(2)
    redis_friends_activity = await aioredis.create_pool(REDIS_URI, db=1)
    redis_coordinates = await aioredis.create_pool(REDIS_URI, db=2)
    app.redis_p = redis_friends_activity
    app.redis_pool_2 = redis_coordinates

    # Словарь с обектами сокетов и id пользователя
    app.wslist = {}
    
    for route in routes_post:
        app.router.add_post(route[0], route[1])

    # Close:
    # client_session
    # web_sockets
    # connection to redis
    app.on_cleanup.append(on_cleanup)

    return app


async def on_cleanup(app):
    # closing redis pool and connection
    app.redis.close()
    await app.wait_closed()

    for ws in app.wslist.value:
        if not ws.closed():
            ws.close()