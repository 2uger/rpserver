from aiohttp.web import View, Request

class Map(web.View):
    async def activate(self):
        app = self.request.app

        user = User(self.request.headers.get('user_id'))
        await user.create_friends_status(app.get('redis_friends_status'))
        await user.create_coordinates(app.get('redis_coordinates'))

        return json_response({'message': 'User activated'}, status=200)


    async def deactivate(self):
        app = self.request.app

        user = User(self.request.rel_url_query.get('user_id'))
        await user.delete_friends_status(app.redis_friends_status)
        await user.delete_coordinates(app.redis_coordinates)

        return json_response({'message': 'User deactivated'}, status=200)


class WebSocket(web.View):
    async def create(self):
        app = self.request.app
        ws = web.WebSocketResponse()
        await ws.prepare(self.request)
        user_id = self.request.rel_url_query.get('user_id')

        user = User(user_id)

        if app.wslist.get(user.id, None):
            raise BadRequest
        else:
            app.wslist[user.id] = ws
        async for msg in ws:
            if msg.type == WSMsgType.TEXT:
                if msg.data = 'close':
                    await ws.close()
                else:

                    CHECK WHAT TYPE MSG.DATA IS

                    await user.update_coordinates(msg.data, app.redis_coordinates)  
                    friends_status = await user.get_friends_status(app.redis_friends_status)
                    friends_coordinates = await user.get_friends_coordinates(friends_status,
                                                                             app.redis_coordinates)

                    ws.send_str(friends_coordinates)
                    CHECK WHAT TYPE SHOULD I SEND VIA WS

            elif msg.type == WSMsgType.ERROR:
                logging
        await self.disconnect(user.id, ws)

    async def disconnect_user(self, user_id, ws):
        pass

