import asyncio
import aioredis

from aiohttp import web

from routes import routes
from middleware import ...
from settings import *


async def create_app():
    app = web.Application(client_max_size=MAX_REQUEST_SIZE, middlewares=middleware)

    #For store user's friends activity(1)
    #and user coordinates(2)
    redis_pool_1 = await aioredis.create_pool(settings.REDIS_URI, db=1)
    redis_pool_2 = await aioredis.create_pool(settings.REDIS_URI, db=2)
    app.redis_pool_1 = redis_pool_1
    app.redis_pool_2 = redis_pool_2

    #Client session for making request
    #to the API for list of friends
    session = ClientSession()
    app.client_session = session

    # Словарь с обектами сокетов и id пользователя
    app.wslist = {}
    
    for route in routes:
        app.router.add_route(**route)

    #Close:
    #--clien_session
    #--web_sockets
    #--connection to redis
    app.on_cleanup.append(on_cleanup)

    return app

async def on_cleanup(app):
    await app.client_session.close()
    
    #closing redis pool and connection
    app.redis.close()
    await app.wait_closed()


