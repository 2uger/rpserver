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

        user = User(request.headers.get('user_id'))
        await user.create_friends_status(app.get('redis_friends_status'))
        await user.create_coordinates(app.get('redis_coordinates'))

        return json_response({'message': 'User activated'}, status=200)

    @staticmethod
    async def deactivate(request):
        app = request.app

        user = User(request.rel_url_query.get('user_id'))
        await user.delete_friends_status(app.redis_friends_status)
        await user.delete_coordinates(app.redis_coordinates)

        return json_response({'message': 'User deactivated'}, status=200)


class WebSocket(object):
    async def create(self, request):
        app = request.app
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        user_id = request.rel_url_query.get('user_id')

        user = User(user_id)

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
                    if not self.check_user_coordinates(user_coordinates):
                        continue
                    await user.update_coordinates(message.data, app.redis_coordinates)
                    friends_status = await user.get_friends_status(app.redis_friends_status)
                    friends_coordinates = await user.get_friends_coordinates(friends_status,
                                                                             app.redis_coordinates)
                    await ws.send_json(friends_coordinates)
            else:
                await self.__disconnect_user(user.id, ws)
                # logging
        await self.__disconnect_user(user.id, ws, app)

    @staticmethod
    async def __disconnect_user(user_id, ws, app):
        ws.send_json({'message': 'WebSocket closed'})
        if not ws.closed():
            ws.close()
        del app.wslist[user_id]

    @staticmethod
    async def __check_user_coordinates(user_coordinates):
        try:
            if user_coordinates['long'] <= 0:
                return None                    
            elif user_coordinates['lat'] <= 0:
                return None
        except KeyError:
            return None
        else:
            return True
        


