from rpserver.api.config.config import DevelopmentConfig
from rpserver.api.app import create_app

# TODO: add command line argument for Config
# --config = dev, prod
def main():
    app = create_app(DevelopmentConfig)
    app.run()


if __name__ == '__main__':
    main()
