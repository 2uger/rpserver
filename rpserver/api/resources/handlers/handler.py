"""
BaseHandler
"""


from sqlalchemy.sql import select


from rpserver.db import engine


class BaseHandler:
    def __init__(self, request):
        pass

    def get(connection, db_table, object_id):
        query = select([db_table]).where(db_table.c.id == object_id)
        try:
            result = connection.execute(query).fetchall()
        except:
            return False
        else:
            return result
        finally:
            result.close()

    def post(connection, db_table, insert_data):
        try:
            connection.execute(db_table.insert(), insert_data)
        except:
            return False
        else:
            return True

    def put(self):
        pass

    def delete(self):
        pass

