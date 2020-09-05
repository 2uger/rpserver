from aiohttp import web

class Map(web.View):
    async def activate(self):
        MAKE REQ TO GET USERS FRIENDS
        db1 {user:{user_f:status}}
        db2 {user:coordinates}
        MAKE DICT WITH USER FRIENDS
        SCAN DB FOR SAME USERS AND MAKE THEM ACTIVE IN USER DICT
        
        async with redis.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(USERS FRIENDS)
                    active_friends = redis.get
                    pipe.unwatch()
                    MAKE DICT WITH USER FRIENDS
                    break
                except redis.WatchError:
                    logging
        MAKE DICT USER:COORDINATE
        ALLOCATE WEBSOCKET

    async def deactivate(self):
        REMOVE FROM DB2
        REMOVE FROM DB1

class WebSocket(web.View):
    async def get(self):
        app = self.request.app
        ws = web.WebSocketResponse()
        await ws.prepare(self.request)
        app.wslist[self.request.RIDER_ID] = ws

