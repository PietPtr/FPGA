import pygame
from consts import *
from math import *
import random

class FPGA:
    def __init__(self, config, width, height):
        self.config = config
        self.width = width
        self.height = height

        self.tiles = []

        for x in range(self.width):
            for y in range(self.height):
                try:
                    lut_config = DEFAULT_LUT_CONFIG | self.config['tiles'][str(x) + ',' + str(y)]
                except KeyError:
                    lut_config = DEFAULT_LUT_CONFIG
                self.tiles.append(Tile(self, lut_config, x, y))

    def draw(self, surface):
        for tile in self.tiles:
            tile.draw(surface)
    
class Tile:
    def __init__(self, fpga, config, x, y):
        self.fpga = fpga
        self.config = config
        self.x = x
        self.y = y

        self.luts = {
            "horz": LUT(self, config['horzLut'], True),
            "vert": LUT(self, config['vertLut'], False)
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

        self.wire_combinators = [
            WireCombinator(self, a_x, q_y),
            WireCombinator(self, p_x, b_y)
        ]

    def dy(self):
        return (self.fpga.height - self.y - 1) * SIZE

    def dx(self):
        return self.x * SIZE

    def pos(self):
        return (self.dx(),self.dy())

    def draw(self, surface):
        pygame.draw.rect(surface, white, pygame.Rect(
            self.dx(), self.dy(), SIZE, SIZE))

        coordinate = font.render(f'{self.x},{self.y}', False, black)
        surface.blit(coordinate, self.pos())

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

        for comb in self.wire_combinators:
            comb.draw(surface)




class LUT:
    def __init__(self, tile, config, horizontal):
        self.tile = tile
        self.config = config
        self.horizontal = horizontal
        (self.width, self.length) = LUTSIZE
        
        self.inputs = {
            "a": Input(self, 0.1 * SIZE, self.width),
            "b": Input(self, self.length - 0.1 * SIZE, 0)
        }

        p_height = self.length - 0.1 * SIZE
        q_height = 0.1 * SIZE
        self.outputs = {
            "p": Output(self, p_height, self.width),
            "q": Output(self, q_height, 0)
        }

        reg_offset = SIZE*0.07
        self.registers = {
            "r0": Register(self, p_height, -reg_offset, self.config['enableReg0']),
            "r1": Register(self, q_height, self.width + reg_offset, self.config['enableReg1'])
        }

        self.x = (SIZE - self.length) * 0.3
        self.y = (SIZE - self.width) - SIZE * 0.1

        randstr = "".join([random.choice("01") for x in [x for x in range(8)]])
        self.config_obj = LUTConfig(self, self.config['lutConfig'])

    def orient(self, x, y, w, h):
        if self.horizontal:
            return (x, y, w, h)
        else:
            return (y, x, h, w)

    # x position relative to parent
    def dx(self):
        return (self.x if self.horizontal else self.y)

    # y position relative to parent
    def dy(self):
        return (self.y if self.horizontal else self.x)

    # always the same i guess
    # position on screen
    def pos(self):
        (px, py) = self.tile.pos()
        return (px + self.dx(), py + self.dy())

    def draw(self, surface):
        x = self.dx()
        y = self.dy()
        l = self.length if self.horizontal else self.width
        w = self.width if self.horizontal else self.length
        
        pygame.draw.rect(surface, black, pygame.Rect(self.pos(), (l, w)), width=LINE)

        self.config_obj.draw(surface)

        for name, input in self.inputs.items():
            input.draw(surface, name)

        for name, output in self.outputs.items():
            output.draw(surface, name)

        for name, register in self.registers.items():
            register.draw(surface, name)


class Connect:
    def __init__(self, parent, x_offset, y_offset):
        self.parent = parent
        self.x_offset = x_offset
        self.y_offset = y_offset

    def dx(self):
        return self.x_offset
    
    def dy(self):
        return self.y_offset

    def pos(self):
        (px, py) = self.parent.pos()
        return (px + self.dx(), py + self.dy())

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
        self._draw(surface, self.pos(), name)

    def _draw(self, surface, pos, name):
        radius = SIZE * 0.03
        (x, y) = pos
        pygame.draw.circle(surface, white, pos, radius)
        pygame.draw.circle(surface, black, pos, radius, width=LINE)

        str = f'{name}'
        inp_name = font.render(str, False, black)
        (fx, fy) = font.size(str)
        surface.blit(inp_name, (x - fx/2, y - fy/2))

class Output(Input):
    def __init__(self, parent, x_offset, y_offset):
        Input.__init__(self, parent, x_offset, y_offset)
    
    def _draw(self, surface, pos, name):
        sidelength = SIZE * 0.053/2
        (x, y) = pos
        rect = pygame.Rect(x - sidelength, y - sidelength, sidelength * 2, sidelength * 2)
        draw_box(surface, rect)

        str = f'{name}'
        inp_name = font.render(str, False, black)
        (fx, fy) = font.size(str)
        surface.blit(inp_name, (x - fx / 2, y - fy / 2))

class WireCombinator:
    def __init__(self, parent, x, y):
        self.tile = parent
        self.x = x
        self.y = y

    def dx(self):
        return self.x
    
    def dy(self):
        return self.y

    def pos(self):
        (px, py) = self.tile.pos()
        return (px + self.dx(), py + self.dy())

    def draw(self, surface):
        radius = SIZE * 0.02
        pygame.draw.circle(surface, black, self.pos(), radius)
    

class Register:
    def __init__(self, parent, x, y, state):
        self.parent = parent

        self.x = x if self.parent.horizontal else y
        self.y = (-y + self.parent.width) if self.parent.horizontal else x
        w = SIZE * 0.053/2
        h = 2 * w
        self.width = w if self.parent.horizontal else h
        self.height = h if self.parent.horizontal else w

        self.state = state

    def dx(self):
        return self.x

    def dy(self):
        return self.y

    def pos(self):
        (px, py) = self.parent.pos()
        return (px + self.dx() - self.width / 2, py + self.dy() - self.height / 2)

    def draw(self, surface, name):
        (x, y) = self.pos()
        rect = pygame.Rect(self.pos(), (self.width, self.height))

        color = gray
        if self.state:
            draw_box(surface, rect)
            color = black

        str = f'{name}'
        inp_name = font.render(str, False, color)
        offset = SIZE * 0.03
        surface.blit(inp_name, (x + offset, y) if self.parent.horizontal else (x, y + offset))


class LUTConfig:
    def __init__(self, parent, config):
        self.parent = parent
        self.config = config
        self.config_r = "".join(list(reversed(config))) # [0] = LSB

        self.a_pos = self.parent.inputs["a"].pos()
        self.b_pos = self.parent.inputs["b"].pos()

    def dy(self):
        return SIZE * 0.005 if self.parent.horizontal else SIZE * 0.14
    def dx(self):
        return SIZE * 0.17 if self.parent.horizontal else SIZE * 0.03

    def pos(self):
        (px, py) = self.parent.pos()
        return (px + self.dx(), py + self.dy())

    def draw(self, surface):
        if self.config in resource_map:
            surf = resource_map[self.config]
            if self.parent.horizontal:
                surf = pygame.transform.rotate(surf, 90)
            surface.blit(surf, self.parent.pos())
        else:
            (x, y) = self.pos()
            for i in range(4):
                text = f'{i:02b}:{self.config_r[i*2:i*2+2]}'
                table_line = font.render(text, False, black)
                (fx, fy) = font.size(text)
                surface.blit(table_line, (x, y + i * fy))

def add_tup(v1, v2):
    (ax, ay) = v1
    (bx, by) = v2
    return (ax + bx, ay + by)