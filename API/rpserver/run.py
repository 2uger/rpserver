from rpserver.config.config import DevelopmentConfig
from rpserver.main import backend_app, auth_app

# TODO: add command line argument for Config
# --config = dev, prod
def main():
    backend = backend_app(DevelopmentConfig)
    #auth = auth_app(DevelopmentConfig)
    backend.run()
    #auth.run()


if __name__ == '__main__':
    main()
