from flask import Blueprint

coordinate = Blueprint('coordinate', __name__)

from . import routes