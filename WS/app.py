import asyncio
import aioredis

from aiohttp import web

from routes import routes
from middleware import ...
from settings import *


async def create_app():
    app = web.Application(client_max_size=MAX_REQUEST_SIZE, middlewares=middleware)

    redis_pool = await aioredis.create_pool(settings.REDIS_URI)
    app.redis_pool = redis_pool

    # Словарь с обектами сокетов и id пользователя
    app.wslist = {}
    
    for route in routes:
        app.router.add_route(**route)

    return app



