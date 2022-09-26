import pygame
from consts import *
from math import *

class FPGA:
    def __init__(self, width, height):
        self.width = width
        self.height = height

        self.tiles = []

        for x in range(self.width):
            for y in range(self.height):
                self.tiles.append(Tile(self, x, y))

    def draw(self, surface):
        for tile in self.tiles:
            tile.draw(surface)
    
class Tile:
    def __init__(self, fpga, x, y):
        self.fpga = fpga
        self.x = x
        self.y = y

        self.luts = {
            "horz": LUT(self, True),
            "vert": LUT(self, False)
        }

        q_y = SIZE * 0.25
        b_y = SIZE * 0.55
        a_x = SIZE * 0.25
        p_x = SIZE * 0.55

        self.connect = {
            "up_in": Connect(self, a_x, SIZE),
            "left_in": Connect(self, SIZE, b_y),
            "down_in": Connect(self, p_x, 0),
            "right_in": Connect(self, 0, q_y),
            "up_out": Connect(self, a_x, 0),
            "left_out": Connect(self, 0, b_y),
            "down_out": Connect(self, p_x, SIZE),
            "right_out": Connect(self, SIZE, q_y)
        }

    def dy(self):
        return (self.fpga.height - self.y - 1) * SIZE

    def dx(self):
        return self.x * SIZE

    def draw(self, surface):
        pygame.draw.rect(surface, white, pygame.Rect(
            self.dx(), self.dy(), SIZE, SIZE))

        coordinate = font.render(f'{self.x},{self.y}', False, (0, 0, 0))
        surface.blit(coordinate, (self.dx(),self.dy()))

        def connect_output(tile_connection, lutname, outputname):
            self.connect[tile_connection].draw_connect(surface, self.luts[lutname].outputs[outputname])
        def connect_input(tile_connection, lutname, outputname):
            self.connect[tile_connection].draw_connect(surface, self.luts[lutname].inputs[outputname])

        connect_output("up_out", "horz", "q")
        connect_input("up_in", "horz", "a")
        connect_output("down_out", "horz", "p")
        connect_input("down_in", "horz", "b")
        connect_output("right_out", "vert", "q")
        connect_input("right_in", "vert", "a")
        connect_output("left_out", "vert", "p")
        connect_input("left_in", "vert", "b")

        for name, lut in self.luts.items():
            lut.draw(surface)

        for _, connect in self.connect.items():
            connect.draw_debug(surface)



class LUT:
    def __init__(self, tile, horizontal):
        self.tile = tile
        self.horizontal = horizontal
        self.length = SIZE * 0.5
        self.width = self.length * 0.4
        
        self.inputs = {
            "a": Input(self, 0.1 * SIZE, self.width),
            "b": Input(self, self.length - 0.1 * SIZE, 0)
        }

        self.outputs = {
            "p": Output(self, self.length - 0.1 * SIZE, self.width),
            "q": Output(self, 0.1 * SIZE, 0)
        }

        self.x = (SIZE - self.length) * 0.3
        self.y = (SIZE - self.width) - SIZE * 0.1

    def dx(self):
        return self.tile.dx() + (self.x if self.horizontal else self.y)

    def dy(self):
        return self.tile.dy() + (self.y if self.horizontal else self.x)

    def draw(self, surface):
        x = self.dx()
        y = self.dy()
        l = self.length if self.horizontal else self.width
        w = self.width if self.horizontal else self.length
        
        pygame.draw.rect(surface, black, pygame.Rect(x, y, l, w), width=LINE)

        for name, input in self.inputs.items():
            input.draw(surface, name)

        for name, output in self.outputs.items():
            output.draw(surface, name)

class Connect:
    def __init__(self, parent, x_offset, y_offset):
        self.parent = parent
        self.x_offset = x_offset
        self.y_offset = y_offset

    def pos(self):
        return (self.parent.dx() + self.x_offset, self.parent.dy() + self.y_offset)

    def draw_connect(self, surface, other):
        pygame.draw.line(surface, black, other.pos(), self.pos())

    def draw_debug(self, surface):
        if debug:
            pygame.draw.circle(surface, green, self.pos(), SIZE * 0.02)

class Input(Connect):
    def __init__(self, parent, x_offset, y_offset):
        Connect.__init__(self, parent, x_offset, y_offset)
        if not self.parent.horizontal:
            self.x_offset = -y_offset + self.parent.width
            self.y_offset = x_offset

    def draw(self, surface, name):
        (x, y) = self.pos()
        self._draw(surface, x, y, name)

    def _draw(self, surface, x, y, name):
        radius = SIZE * 0.03
        pygame.draw.circle(surface, white, (x, y), radius)
        pygame.draw.circle(surface, black, (x, y), radius, width=LINE)

        str = f'{name}'
        inp_name = font.render(str, False, (0, 0, 0))
        (fx, fy) = font.size(str)
        surface.blit(inp_name, (x - fx/2, y - fy/2))

class Output(Input):
    def __init__(self, parent, x_offset, y_offset):
        Input.__init__(self, parent, x_offset, y_offset)
    
    def _draw(self, surface, x, y, name):
        sidelength = SIZE * 0.053/2
        rect = pygame.Rect(x - sidelength, y - sidelength, sidelength * 2, sidelength * 2)
        pygame.draw.rect(surface, white, rect)
        pygame.draw.rect(surface, black, rect, width=LINE)

        str = f'{name}'
        inp_name = font.render(str, False, (0, 0, 0))
        (fx, fy) = font.size(str)
        surface.blit(inp_name, (x - fx / 2, y - fy / 2))