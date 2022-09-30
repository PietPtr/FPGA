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

# Maps bitvectors to their image representation
resource_map_names = {
    "00000000": "empty.png",
    "01010000": "forward_a.png",
    "10001000": "forward_b.png",
    "11011000": "forwardBoth.png",
    "11100100": "turnAround.png",
    "00011011": "invertedTurnAround.png",
    "00010001": "q_is_not_b.png",
    "00001010": "p_is_not_a.png",
    "10100000": "p_is_a.png",
    "01000100": "q_is_b.png"
}

resource_map = {}

LUTSIZE = (SIZE * 0.5 * 0.4, SIZE * 0.5)

for bv, filename in resource_map_names.items():
    surf = pygame.image.load("resources/" + filename)
    print(f"Loaded {filename}")
    resource_map[bv] = pygame.transform.scale(surf, LUTSIZE)

DEFAULT_LUT_CONFIG = {
    "horzLut": {
        "lutConfig": "00000000",
        "enableReg1": True,
        "enableReg0": True
    },
    "vertLut": { 
        "lutConfig": "00000000", 
        "enableReg1": True, 
        "enableReg0": True }
}