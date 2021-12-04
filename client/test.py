import pygame
import random

BLACK = (0,0,0)
WHITE = (255,255,255)
 

class Rider(pygame.sprite.Sprite):    
    def __init__(self, color, width, height):
        super().__init__()
        
        self.image = pygame.Surface([width, height])
        self.image.fill(BLACK)
        self.image.set_colorkey(BLACK)
        pygame.draw.rect(self.image, color, [0, 0, width, height])
        
        self.rect = self.image.get_rect()

    def m_down(self):
        self.rect.y += 1
    
    def m_right(self):
        self.rect.x += 1

    def coords(self):
        return (self.rect.x, self.rect.y)


pygame.init()
 
 
# Open a new window
size = (800, 800)
screen = pygame.display.set_mode(size)
pygame.display.set_caption("Coordinates")
 
me = Rider(WHITE, 10, 10)
me.rect.x = 20
me.rect.y = 200

 
all_sprites_list = pygame.sprite.Group()
 
all_sprites_list.add(me)
 
clock = pygame.time.Clock()
 
run = True

while run:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            run = False
        elif event.type==pygame.KEYDOWN:
            if event.key==pygame.K_x:
                run = False
        elif event.type == pygame.KEYUP:
            pad = Rider(WHITE, 10, 10)
            pad.rect.x = random.randint(0, 500)
            pad.rect.y = random.randint(0, 500)
            all_sprites_list.add(pad)
 
    # all update should be there
    keys = pygame.key.get_pressed()
    if keys[pygame.K_DOWN]:
        me.m_down()
    if keys[pygame.K_RIGHT]:
        me.m_right()

    all_sprites_list.update()
    print(me.coords())
 
    screen.fill(BLACK)
    #Draw the net
    pygame.draw.line(screen, WHITE, [349, 0], [349, 500], 5)
    
    #Now let's draw all the sprites in one go. (For now we only have 2 sprites!)
    all_sprites_list.draw(screen) 
 
    # --- Go ahead and update the screen with what we've drawn.
    pygame.display.flip()
     
    # --- Limit to 60 frames per second
    clock.tick(60)
 
pygame.quit()
