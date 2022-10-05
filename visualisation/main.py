import pygame
from fpga import *
from consts import *
import json
from pprint import pprint

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

config = load("config.json")
fpga = FPGA(config, 4, 4)

pprint(config)

save(draw(fpga), "out.png")
