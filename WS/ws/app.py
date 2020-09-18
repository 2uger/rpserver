import asyncio
import aioredis

from aiohttp import web

from routes import routes_post
from middleware import check_access_token, error_middleware 
from settings import *


async def create_app():
    app = web.Application(client_max_size=MAX_REQUEST_SIZE, 
                          middlewares=[check_access_token, error_middleware])

    #For store user's friends activity(1)
    #and user coordinates(2)
    redis_pool_1 = await aioredis.create_pool(settings.REDIS_URI, db=1)
    redis_pool_2 = await aioredis.create_pool(settings.REDIS_URI, db=2)
    app.redis_pool_1 = redis_pool_1
    app.redis_pool_2 = redis_pool_2


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
    #closing redis pool and connection
    app.redis.close()
    await app.wait_closed()

    for ws in app.wslist.value:
        if not ws.closed():
            ws.close()
