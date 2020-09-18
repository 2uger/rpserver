from map.views import Map, Websocket


user_map = Map()
ws = WebSocket() 

routes_post = [
        ('/activate_user_map', user_map.activate),
        ('/deactivate_user_map', user_map.deactivate),
        ('/create_user', ws.create),
]
