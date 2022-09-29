import pygame
from fpga import *
from consts import *
import json

def save(surface, name):
    pygame.image.save(surface, name)

def draw(fpga):
    surface = pygame.Surface((fpga.width * SIZE, fpga.height * SIZE))
    fpga.draw(surface)
    return surface

def load(config_name):
    with open(config_name, "r") as file:
        config = json.load(file)
        return config

fpga = FPGA(load("config.json"), 4, 4)

save(draw(fpga), "out.png")
