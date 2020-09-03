from aiohttp import web
from aiohttp.web import middleware


@middleware
async def token_auth(request, handler):
    TOKEN CHECK
    if OK:
        handler(request)
    else:
        return 'LOGGING FIRST'

