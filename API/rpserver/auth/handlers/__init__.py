from flask import Blueprint

auth_bp = Blueprint('auth', __name__)

from . import login, logout, registration, token

