from app import app

@app.route('/')
def index():
    return "How r u?"
