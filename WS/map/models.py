TODO:
    -make id property
    -methods to create user 


class User():
    """User model to use at Map.

    :param id: User identificator(id from database)
    """

    def __init__(self, id):
        self.id = id

    async def get_friend(self):
        """
        :rtype: Dict {friend_1_id: status,
                      friend_2_id: status}
        """
        async with request('GET', 
                           'https://API NAME HOST/user/friends',
                           params=params,
                           headers['acess_token': access_token]) as resp:
            if resp.status == 401 or resp.status != 200:
                return resp.json()
            return resp.json() 


    async def create_friends_status(self, redis, friends):
        active_friends = {}
        async with redis.pipeline() as pipe:
            while True:
                try:
                    pipe.watch(*friends)
                    for friend_id, status in friends.items():
                        await status = redis.hget(friend_id, f'{friend_id}')
                        if status == 1:
                            active_friends[friend_id] = status
                        else:
                            del active_friends[friend_id]
                    pipe.multi()
                    pipe.hmset_dict({self.id: active_friends})
                    await pipe.execute()
                    pipe.unwatch()
                    break
                except aioredis.WatchError:
                    LOGGIN
        return True

    async def create_coordinates_note(self, redis_coordinates):
        await redis_coordinates.hmset_dict(self.id, {'lng': 0,
                                                     'lat': 0})

    async def get_user_coordinate(self):
        pass

    async def update_coordinates(self):
        pass
