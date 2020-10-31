make createvenv
python3 setup.py install
cd rpserver
alembic upgrade head
