from flask import current_app


from ridersPlatform.api import create_app


app = create_app()
if __name__ == '__main__':
    app.run()