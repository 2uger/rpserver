from sqlalchemy import create_engine
from sqlalchemy.orm import scoped_session, sessionmaker


from ridersPlatform.db.models import metadata


engine = create_engine('postgresql://thuger_db:thuger_db@localhost:ridersplatform')
#db_session = scoped_session(autocommit=False,
#                            autoflush=False,
#                            bind=engine)


def init_db():
    metadata.create_all(engine)
