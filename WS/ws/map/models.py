TODO:
    -make id property
    -methods to create user 

from aiohttp.web.exc import HTTPBadRequest as BadRequest


class User():
    """User model to use at Map.

    :param id: User identificator(id from database)
    """

    def __init__(self, id):
        self._id = id

    @property
    async def id(self):
        return self._id

    @property.setter
    async def id(self, user_id):
        if user_id < 1:
            raise BadRequest
        self._id = user_id

    @propery.getter
    async def id(self):
        return self_id

    async def get_friends_status(self, redis_fs):
        pass

    async def __get_friends(self):
        """
        :rtype: Dict {friend_1_id: 0,
                      friend_2_id: 0}
        """
        is_user_exist = 
        async with request('GET', 
                           'https://API NAME HOST/user/friends',
                           params=params,
                           headers['acess_token': access_token]) as resp:
            if resp.status == 401 or resp.status != 200:
                raise BadRequest
            elif resp.status == 302:

            !!!!    raise HTTPFound(location = resp.LOCATION)

            return self.__friends_status = resp.json() 

    async def create_friends_status(self, redis_fs):
        self.__get_friends()
        async with redis.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(*friends)
                    for friend_id, status in friends.items():
                        await status = redis.hget(friend_id, f'{friend_id}')
                        if status == 1:
                            self.active_friends[friend_id] = int(status)
                    pipe.multi()
                    pipe.hmset_dict({self.id: self.active_friends})
                    await pipe.execute()
                    pipe.unwatch()
                    break
                except aioredis.WatchError:
                    LOGGIN
        return True

    async def delete_friends_status(self, redis_fs):
        pass

    async def delete_coordinates(self, redis_coordinates):
        pass

    async def create_coordinates_note(self, redis_coordinates):
        await redis_coordinates.hmset_dict(self.id, {'lng': 0,
                                                     'lat': 0})

    async def update_coordinates(self, coordinates, redic_c):
        pass

    async def get_friends_coordinates(self):
        pass

