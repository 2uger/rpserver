from aiohttp import web
from aiohttp.web import middleware


@middleware
async def check_access_token(request: Request, handler):
    if not access_token = request.headers.get('access_token'):
        json_response({'error': {'message': 'No access_token'}}, status=400)
    try:
        decode_access_token(access_token)
    except jwt.ExpiredSignatureError:
        raise HTTPFound()
        json_response({'error': {'message': 'Token expire'}}, status=400)


def format_http_error(http_error_cls, message: Optional[str] = None,
                      error_fields: Optional[dict] = None) -> HTTPException:
    status_code = HTTPStatus(http_error_cls.status_code)
    error_message = {
            'message': message or status_description
    }
    if error_fields:
        error_message['fields'] = fields

    return http_error_cls(body={'error': error_message})


@middleware
async def error_middleware(request: Request, handler):
    try:
        return await handler(request)
    except 1:
        pass
    except 2:
        pass
    except Exception:
        pass
