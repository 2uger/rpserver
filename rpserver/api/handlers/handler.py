"""
BaseHandler
"""


from sqlalchemy.sql import select, update, delete


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
        query = insert([db_table]).values(inser_data)
        try:
            connection.execute(query)
        except:
            return False
        else:
            return True

    def patch(self, connection, db_table, object_id, update_data):
        query = update([db_table]).where(db_table.c.id==object_id).values(update_data)
        try:
            connection.execute(query)
        except:
            return False
        else:
            return True

    def delete(self, db_table, object_id):
        query = delete([db_table]).where(db_table.c.id==object_id)
        try:
            connection.execute(query)
        except:
            return False
        else:
            return True

