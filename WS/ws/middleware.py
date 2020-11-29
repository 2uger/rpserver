from typing import Optional
from http import HTTPStatus

from aiohttp.web import middleware, json_response
from aiohttp.web_exceptions import HTTPException, HTTPFound, HTTPBadRequest
from aiohttp.web_request import Request
# import jwt
from auth_utils import decode_access_token


def format_http_error(http_error_cls, message: Optional[str] = None,
                      error_fields: Optional[dict] = None) -> HTTPException:
    status_description = HTTPStatus(http_error_cls.status_code)
    error_message = {
            'message': message or status_description
    }
    if error_fields:
        error_message['fields'] = error_fields

    return http_error_cls(body={'error': error_message})


@middleware
async def check_access_token(request, handler):
    if not request.headers.get('access_token'):
        json_response({'error': {'message': 'No access_token'}}, status=400)
    access_token = request.headers.get('access_token')
    try:
        decode_access_token(access_token)
    except jwt.ExpiredSignatureError:
        raise format_http_error(HTTPFound, 'Token expired')


@middleware
async def error_middleware(request: Request, handler):
    print('Eroor middleware')
    try:
        return await handler(request)
    # except HTTPFound as f:
    #     raise format_http_error(HTTPFound, 'Token expired')
    # except HTTPBadRequest as br:
    #     raise format_http_error(br)
    except Exception as e:
        raise e
        return json_response({"msg": e})
