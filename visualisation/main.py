import pygame
from fpga import *
from consts import *

def save(surface, name):
    pygame.image.save(surface, name)

def draw(fpga):
    surface = pygame.Surface((fpga.width * SIZE, fpga.height * SIZE))
    fpga.draw(surface)
    return surface

fpga = FPGA(4, 4)

save(draw(fpga), "out.png")
