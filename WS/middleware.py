from aiohttp import web
from aiohttp.web import middleware


@middleware
async def token_auth(request, handler):
    TOKEN CHECK

    if OK:
        try:
            handler(request)
        except EXCEPTION(validation_error, bad_request_error)
    else:
        return 'LOGGING FIRST'

