from rpserver.api.config.config import DevelopmentConfig
from rpserver.api.app import create_app


def main():
    app = create_app(DevelopmentConfig)
    app.run(port=9090)


if __name__ == '__main__':
    main()
