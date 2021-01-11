from rpserver.config.config import DevelopmentConfig
from rpserver.main import api_app


# TODO: add command line argument for Config
# --config = dev, prod
def main():
    api = api_app(DevelopmentConfig)
    api.run(port=8081)


if __name__ == '__main__':
    main()
