from app import app, db
import view


from spots.blueprint import spots


app.register_blueprint(spots, url_prefix='/spots')

if __name__ == "__main__":
    app.run()
