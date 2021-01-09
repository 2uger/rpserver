from inspect import ismethod
from flask import Flask, request


app = Flask(__name__)

def call(obj, *a, **kw):
    for x in dir(request):
        attr = getattr(obj, x)
        print(attr)
    
@app.route('/test', methods=["GET"])
def ge():
    print(request)
    print(dir(request))
    call(request, "my input")
    return "Oleg"

if __name__ == "__main__":
    app.run()
