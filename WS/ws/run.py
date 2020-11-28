"""
Точка входа для запуска приложения с заданными аргументами
"""
from aiomisc import bind_socket
from aiohttp.web import run_app

from app import create_app

from config.settings import HOST, PORT


def main():
    socket = bind_socket(address=HOST, port=PORT, proto_name='http')
    app = create_app()
    run_app(app, sock=socket)


if __name__ == '__main__':
    main()