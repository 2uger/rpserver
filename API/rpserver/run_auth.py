from rpserver.config.config import DevelopmentConfig
from rpserver.main import auth_app


# TODO: add command line argument for Config
# --config = dev, prod
def main():
    auth = auth_app(DevelopmentConfig)
    auth.run(port=8080)


if __name__ == '__main__':
    main()
