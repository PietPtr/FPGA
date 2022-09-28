import pygame


SIZE = 1320 / 4 # arbitrary size constant, equal to the sides of a Tile
LINE = round(SIZE / 200)

pygame.init()

pygame.font.init()
font = pygame.font.SysFont("Courier", int((SIZE / 200) * 10))

NIGHT = True

def n(r, g, b):
    global NIGHT
    if NIGHT:
        return (255 - r, 255 - g, 255 - b)
    else:
        return (r, g, b)

white = n(255-25, 255-25, 255-25)
black = n(0, 0, 0)
red = n(255, 0, 0)
green = n(0, 255, 0)
gray = n(127, 127, 127)

debug = False

def draw_box(surface, rect):
        pygame.draw.rect(surface, white, rect)
        pygame.draw.rect(surface, black, rect, width=LINE)