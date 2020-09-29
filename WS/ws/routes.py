from ws.map.views import Map, WebSocket


routes_post = [
        ('/activate_user_map', Map.activate),
        ('/deactivate_user_map', Map.deactivate),
        ('/create_user', WebSocket.create),
]
