import json

from aiohttp import web
from aiohttp.web import json_response
from aiohttp.web_request import Request
from aiohttp.web_exceptions import HTTPBadRequest
from aiohttp.http_websocket import WSMsgType

from map.models import User


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

        user_id = request.rel_url_query.get('user_id')
        await User.deactivate(user_id, app.get('db_friends_status'))
        await User.delete_coordinates(user_id, app.get('db_coordinates'))

        return json_response({'message': 'User deactivated'}, status=200)


class WebSocket(object):

    @staticmethod
    async def create(request):
        app = request.app
        print(request.host)
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        rider_id = int(request.match_info.get("rider_id"))
        
        app.connections[rider_id] = ws
        app.coordinates[rider_id] = None
        while True:
            msg = await ws.receive()
            if msg.type == WSMsgType.TEXT:
                if msg.data == "close":
                    await WebSocket._disconnect_user(rider_id, app)
                rider_coordinates = msg.json()
                app.coordinates[rider_id] = rider_coordinates
                await WebSocket._broadcast_message(rider_id, app)
                    # else:
                    #     if not _is_correct_coordinates(user_coordinates):
                    #         ws.send_json({'error': {'message': 'Wrong coordinates'}})
                    #     await User.update_coordinates(message.data, app.redis_coordinates)
                    #     friends_status = await User.get_friends_status(user_id, app.redis_friends_status)
                    #     friends_coordinates = await User.get_friends_coordinates(user_id,
                    #                                                              friends_status,
                    #                                                              app.redis_coordinates)
                    #     await ws.send_json(friends_coordinates)
            else:
                break
    
        await WebSocket._disconnect_user(rider_id, app)

    @staticmethod
    async def _broadcast_message(rider_id, app):
        friends_coord = {}
        for r_id, coord in app.coordinates.items():
            if r_id == rider_id:
                continue
            friends_coord[r_id] = coord
        await app.connections[rider_id].send_json(friends_coord)

    @staticmethod
    async def _disconnect_user(rider_id, app):
        del app.coordinates[rider_id]
        await app.connections[rider_id].send_json({'message': 'WebSocket closed'})
        await app.connections[rider_id].close()
        del app.connections[rider_id]

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
        


