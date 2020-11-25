import json

from aiohttp import web
from aiohttp.web import json_response
from aiohttp.web_request import Request
from aiohttp.web_exceptions import HTTPBadRequest
from aiohttp.http_websocket import WSMsgType

from ws.map.models import User


class Map(object):

    @staticmethod
    async def activate(request):
        app = request.app

        user_id = request.rel_url_query.get('user_id')
        await User.activate(user_id, app.get('db_friends_status'))
        await User.create_coordinates(user_id, app.get('db_coordinates'))

        return json_response({'message': 'User activated'}, status=200)

    @staticmethod
    async def deactivate(request):
        app = request.app

        user_id = request.rel_url_query.get('user_id'))
        await User.deactivate(user_id, app.get('db_friends_status'))
        await User.delete_coordinates(user_id, app.get('db_coordinates'))

        return json_response({'message': 'User deactivated'}, status=200)


class WebSocket(object):

    @staticmethod
    async def create(request):
        app = request.app
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        user_id = request.rel_url_query.get('user_id')


        if app.wslist.get(user.id, None):
            raise HTTPBadRequest
        else:
            app.wslist[user.id] = ws
        async for message in ws:
            if message.type == WSMsgType.TEXT:
                try:
                    user_coordinates = json.loads(message)
                except json.ERROR:
                    ws.send_json({'message': 'Wrong type'})
                    continue
                else:
                    if not _is_correct_coordinates(user_coordinates):
                        ws.send_json({'error': {'message': 'Wrong coordinates'}})
                    await User.update_coordinates(message.data, app.redis_coordinates)
                    friends_status = await User.get_friends_status(user_id, app.redis_friends_status)
                    friends_coordinates = await User.get_friends_coordinates(user_id,
                                                                             friends_status,
                                                                             app.redis_coordinates)
                    await ws.send_json(friends_coordinates)
            else:
                break
        await _disconnect_user(user.id, ws, app)

    @staticmethod
    async def _disconnect_user(user_id, ws, app):
        ws.send_json({'message': 'WebSocket closed'})
        if not ws.closed():
            ws.close()
        del app.wslist[user_id]

    @staticmethod
    async def _is_correct_coordinates(user_coordinates):
        try:
            if user_coordinates['long'] <= 0:
                return None                    
            elif user_coordinates['lat'] <= 0:
                return None
        except KeyError:
            return None
        else:
            return 1
        


