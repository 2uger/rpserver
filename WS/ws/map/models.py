from aiohttp import request
from aiohttp.web import Request
from aiohttp.web_exceptions import HTTPBadRequest as BadRequest, HTTPFound
import aioredis

from ws.config.settings import API_HOST_NAME


class User(object):
    """User model to use at Map.

    :param id: User identificator(id from database)
    """

    def __init__(self, user_id):
        self._id = user_id

    @property
    async def id(self):
        return self._id

    @id.setter
    async def id(self, user_id):
        if user_id < 1:
            raise BadRequest
        self._id = user_id

    @id.getter
    async def id(self):
        return self._id

    async def get_friends_status(self, redis_fs):
        """
        : rtype: Dict {friend_id_1: status,
                       friend_id_2: status}
        """
        pass

    @staticmethod
    async def __get_friends(user_request: Request):
        """
        :rtype: Dict {user_id:[friends_ids]}
        """
        user_access_token = user_request.headers.get('access_token')
        async with request('GET', 
                           f'https://{API_HOST_NAME}/user/friends',
                           headers={'acess_token': user_access_token}) as resp:
            if resp.status == 401 or resp.status != 200:
                raise BadRequest
            elif resp.status == 302:
                raise HTTPFound(location=resp.LOCATION)
            return resp.json() 

    async def create_friends_status(self, redis_fs, user_request: Request):
        """
        Create dict in redis_friends_status {friend_1_id: status,
                                             friend_2_id: status}
        Set status = 1 in every friends dict for self id
        """

        friends = self.__get_friends(user_request)
        friends_status = {}
        async with redis_fs.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(*friends)
                    for friend_id in friends:
                        status = await redis_fs.hget(friend_id, f'{friend_id}')
                        if int(status)== 1:
                            friends_status[friend_id: status]
                    pipe.multi()
                    pipe.hmset_dict({self.id: friends_status})
                    for friend_id in friends.items():
                        await pipe.hset(friend_id, self.id, 1)
                    await pipe.execute()
                    pipe.unwatch()
                    break
                except aioredis.WatchError:
                    pass
                    #LOGGIN
        return 

    async def delete_friends_status(self, redis_fs):
        """
        Delete user dict with friends status
        Set status = 0 in every friends dict for self id
        """

        local_friends_status = self.get_friends_status()
        async with redis_fs.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(*local_friends_status)
                    pipe.multi()
                    pipe.delete(self.id)
                    for friend_id in local_friends_status.items():
                        await pipe.hset(friend_id, self.id, 0)
                    pipe.execute()
                    pipe.unwatch()
                    break
                except aioredis.WatchError:
                    pass
                    # LOGIN
        return 

    async def delete_coordinates(self, redis_coordinates):
        await redis_coordinates.delete(self.id)

    async def create_coordinates(self, redis_coordinates):
        await redis_coordinates.hmset_dict(self.id, {'lng': 0,
                                                     'lat': 0})

    async def update_coordinates(self, new_coordinates, redis_coordinates):
        await redis_coordinates.hmset_dict(self.id, new_coordinates)

    async def get_friends_coordinates(self, redis_coordinates):
        friends_status = self.get_friends_status(redis_coordinates)
        friends_coordinates = {}
        while True:
            try:
                redis_coordinates.watch(*friends_status)
                for friend in friends_status:
                    friends_coordinates[friend] = redis_coordinates.hgetall(friend)
                redis_coordinates.unwatch()
                break
            except aioredis.WatchError:
                pass
                # LOGGIN
        return friends_coordinates
