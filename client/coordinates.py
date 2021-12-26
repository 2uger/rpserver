import time
import pygame
import requests

import config

sendCoordinates = []
recvCoordinates = []


class Rider(pygame.sprite.Sprite):    
    def __init__(self, color, width, height, rider_id):
        super().__init__()
        self.id = rider_id
        
        self.image = pygame.Surface([width, height])
        self.image.fill(config.BLACK)
        self.image.set_colorkey(config.BLACK)
        pygame.draw.rect(self.image, color, [0, 0, width, height])
        
        self.rect = self.image.get_rect()

    def m_up(self):
        self.rect.y += -1

    def m_down(self):
        self.rect.y += 1
    
    def m_left(self):
        self.rect.x += -1

    def m_right(self):
        self.rect.x += 1

    def coords(self):
        return (float(self.rect.x), float(self.rect.y))

    def __str__(self):
        return f'Rider {self.id} with {self.coords()}'


def parse_recv_msg(resp):
    resp = resp.split(':')
    rider_id = resp[0]
    coords = map(float, resp[1].split(';'))
    return (rider_id, [x for x in coords])


def start_moving():
    # Open a new window
    while not config.UID:
        time.sleep(1)
        continue
    my_id = config.UID
    size = (800, 800)
    screen = pygame.display.set_mode(size)
    pygame.display.set_caption("Coordinates")
     
    me = Rider(config.WHITE, 10, 10, my_id)
    me.rect.x = 20
    me.rect.y = 200
     
    all_sprites_list = pygame.sprite.Group()
    all_sprites_list.add(me)
     
    clock = pygame.time.Clock()
 
    pygame.init()
 
    run = True
    while run:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                run = False
            elif event.type==pygame.KEYDOWN:
                if event.key==pygame.K_x:
                    run = False
            elif event.type == pygame.KEYUP:
                pass
     
        # all update should be there
        keys = pygame.key.get_pressed()
        me_updated = False
        if keys[pygame.K_BACKSPACE]:
            resp = requests.get('http://0.0.0.0:8000/api/riders')
            riders = resp.json()['resp']
            for rider in riders:
                print(rider['nickname'], ' ', rider['hometown'])
            rider_choice = input(str)
            flw_uid = riders[int(rider_choice)]['uuid']
            print('You want to follow ', flw_uid)
            sendCoordinates.append(f'follow:{flw_uid}')
        if keys[pygame.K_UP]:
            me.m_up()
            me_updated = True
        if keys[pygame.K_DOWN]:
            me.m_down()
            me_updated = True
        if keys[pygame.K_RIGHT]:
            me.m_right()
            me_updated = True
        if keys[pygame.K_LEFT]:
            me.m_left()
            me_updated = True

        if me_updated:
            my_coords = me.coords()
            if len(sendCoordinates) == 1:
                sendCoordinates.pop(0)
            sendCoordinates.append(f'({my_coords[0]};{my_coords[1]})')

        try:
            if len(recvCoordinates) > 0:
                r_id, r_coords = parse_recv_msg(recvCoordinates.pop(0))
                rider_exists = False
                for sprite in all_sprites_list:
                    if sprite.id == r_id:
                        sprite.rect.x = r_coords[0]
                        sprite.rect.y = r_coords[1]
                        all_sprites_list.remove(sprite)
                        all_sprites_list.add(sprite)
                        rider_exists = True
                if not rider_exists:
                    r_sprite = Rider(config.WHITE, 10, 10, r_id)
                    r_sprite.rect.x = r_coords[0]
                    r_sprite.rect.y = r_coords[1]
                    all_sprites_list.add(r_sprite)
        except Exception as e:
            print(e)

        all_sprites_list.update()
     
        screen.fill(config.BLACK)

        all_sprites_list.draw(screen) 
     
        # --- Go ahead and update the screen with what we've drawn.
        pygame.display.flip()
         
        # --- Limit to 60 frames per second
        clock.tick(60)
    pygame.quit()
