from aiohttp import request
from aiohttp.web import Request
from aiohttp.web_exceptions import HTTPBadRequest as BadRequest, HTTPFound
import aioredis

from ws.config.settings import API_HOST_NAME


class User(object):
    """ User model """

    @staticmethod
    async def get_friends_status(user_id, db_conn):
        """
        : rtype: Dict {friend_id_1: status,
                       friend_id_2: status}
        """
        pass

    @staticmethod
    async def _get_friends(user_request: Request):
        """
        :rtype: Dict {user_id:[friends_ids]}
        """
        user_access_token = user_request.headers.get('access_token')
        async with request('GET', 
                           f'https://{API_HOST_NAME}/user/friends',
                           headers={'acess_token': user_access_token}) as resp:
            if resp.status == 401 or resp.status != 200:
                raise BadRequest

            # It means that user token is expired
            # We need to make him update his token
            elif resp.status == 302:
                raise HTTPFound(location=resp.LOCATION)
            return resp.json() 

    @staticmethod
    async def activate(user_id, db_conn, user_request: Request):
        """

        Scanning for active_friends 
        |
        |  
        Create dict in db {user_id: {friend_1_id: status,
                                     friend_2_id: status}}
        |
        |
        Set status = 1 in every friends dict for user_id

        """

        friends = await _get_friends(user_request)
        active_friends = {}
        async with db_conn.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(*friends)
                    for friend_id in friends:
                        status = await redis_fs.hget(friend_id, f'{friend_id}')
                        if int(status)== 1:
                            active_friends[friend_id: status]


                    pipe.multi()
                    pipe.hmset_dict({user_id: active_friends})


                    for friend_id in friends.items():
                        await pipe.hset(friend_id, user_id, 1)
                    await pipe.execute()
                    pipe.unwatch()
                    break
                except aioredis.WatchError:
                    pass
        return 

    @staticmethod
    async def deactivate(user_id, db_conn):
        """
        Delete user dict with friends status
        Set status = 0 in every friends dict for self id
        """

        local_friends_status = _get_friends_status(user_id, db_conn)
        async with redis_fs.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(*local_friends_status)
                    pipe.multi()
                    pipe.delete(user_id)
                    for friend_id in local_friends_status.items():
                        await pipe.hset(friend_id, user_id, 0)
                    pipe.execute()
                    pipe.unwatch()
                    break
                except aioredis.WatchError:
                    pass
                    # LOGIN
        return 

    @staticmethod
    async def delete_coordinates(user_id, db_conn):
        await db_conn.delete(user_id)

    @staticmethod
    async def create_coordinates(user_id, db_conn):
        await db_conn.hmset_dict(user_id, {'lng': 0,
                                           'lat': 0})

    @staticmethod
    async def update_coordinates(user_id, new_coordinates, db_conn):
        await db_conn.hmset_dict(user_id, new_coordinates)

    @staticmethod
    async def get_friends_coordinates(user_id, db_conn):
        friends_status = get_friends_status(db_conn)
        friends_coordinates = {}
        while True:
            try:
                db_conn.watch(*friends_status)
                for friend in friends_status:
                    friends_coordinates[friend] = db_conn.hgetall(friend)
                redis_coordinates.unwatch()
                break
            except aioredis.WatchError:
                pass
                # LOGGIN
        return friends_coordinates
