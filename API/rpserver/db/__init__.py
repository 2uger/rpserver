from flask import current_app
from sqlalchemy import create_engine
from sqlalchemy.orm import scoped_session, sessionmaker


from rpserver.db.schema import metadata


engine = create_engine(current_app.config.get('DB_SERVER_URI'))
#db_session = scoped_session(autocommit=False,
#                            autoflush=False,
#                            bind=engine)


def init_metadata_db():
    metadata.create_all(engine)
