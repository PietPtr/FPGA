import pygame


SIZE = 500 # arbitrary size constant, equal to the sides of a Tile
LINE = round(SIZE / 200)

pygame.init()

pygame.font.init()
font = pygame.font.SysFont("Courier", int((SIZE / 200) * 10))


white = (255, 255, 255)
black = (0, 0, 0)
red = (255, 0, 0)
green = (0, 255, 0)

debug = False