from aiohttp.web import View, Request

class Map(web.View):
    async def activate(self):
        user_id = self.request.headers.get('user_id')
        access_token = self.request.headers.ger('access_token')
        user = User(user_id)
        user_friends = user.get_friends()

        app = self.request.app
        user.create_friends_status(app.get('redis_friends_status'), user_friends)

        await user.create_coordinates_note(self, app.get('redis_coordinates')):


    async def deactivate(self):
        REMOVE FROM DB2
        REMOVE FROM DB1

class WebSocket(web.View):
    async def create(self):
        app = self.request.app
        ws = web.WebSocketResponse()
        await ws.prepare(self.request)
        user_id = self.request.rel_url_query.get('user_id')

        !!!MAKE CORRECT GET USER
        self.user = User.get(user_id)

        app.wslist[user_id] = ws
        async for msg in ws:
            if msg.type == WSMsgType.TEXT:
                if msg.data = 'close':
                    await ws.close()
                else:
                    GET MESSAGE
                    self.user.update_coordinates()
                    SCAN USERS FRIENDS COORD
                    SEND USER BACK DICT WITH NEW FRIENDS COORDINATES
            elif msg.type == WSMsgType.ERROR:
                logging
                break
        await DISCONNECT USER


    async def disconnect_user(self, user_id, ws):
        pass

