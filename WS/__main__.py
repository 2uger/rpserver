"""
Точка входа для запуска приложения с заданными аргументами
"""
from aiomisc import bind_socket

from WS.app import create_app

from WS.settings import HOST, PORTS ...


def main():
    socket = bind_socket(adress=HOST, port=PORT, proto_name='http')
    app = create_app()
    run_app(app, sock=sock)
