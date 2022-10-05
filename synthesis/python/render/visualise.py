import pygame

pygame.init()
font = pygame.font.SysFont(None, 15)
count = 0

WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (100, 100, 100)

def getBackground():
    return pygame.image.load("render/tile.png")


def save(surface, name):
    pygame.image.save(surface, name)


def visualise(grid):
    max_x = max([b.x for b in grid.blocks.keys()])
    max_y = max([b.y for b in grid.blocks.keys()])
    background = getBackground()
    factor_x, factor_y = 160, 160
    surface = pygame.Surface(((max_x + 1) * factor_x, (max_y + 1) * factor_y))
    surface.fill((255, 255, 255))
    for x in range(max_x + 1):
        for y in range(max_y + 1):
            surface.blit(background, (x * factor_x, y * factor_y))

    # Draw different nets
    for location, net in grid.nets.items():
        text = net.assignment if net.assignment is not None else ""
        img = font.render(text, True, (0, 0, 0))
        if location.kind == 0:
            delta_x, delta_y = 80, 20
        else:
            delta_x, delta_y = 10, 100
        surface.blit(img, (location.x * factor_x + delta_x, location.y * factor_y + delta_y))

    for location, tile in grid.blocks.items():
        text = f"{location.x}:{location.y}:{location.kind}"
        img = font.render(text, True, (0, 0, 0))
        surface.blit(img, (location.x * factor_x + 10, location.y * factor_y + 10))

        text = str([x.name for x in tile.bot.states])
        img = font.render(text, True, (0, 0, 0))
        surface.blit(img, (location.x * factor_x + 10, location.y * factor_y + 130))
        text = str(id(tile.bot))
        img = font.render(text, True, (0, 0, 0))
        surface.blit(img, (location.x * factor_x + 10, location.y * factor_y + 140))

        text = str([x.name for x in tile.right.states])
        img = font.render(text, True, (0, 0, 0))
        img = pygame.transform.rotate(img, 270)
        surface.blit(img, (location.x * factor_x + 130, location.y * factor_y + 10))
        text = str(id(tile.right))
        img = font.render(text, True, (0, 0, 0))
        img = pygame.transform.rotate(img, 270)
        surface.blit(img, (location.x * factor_x + 140, location.y * factor_y + 10))



    global count
    name = f"result{count}"
    print(name)
    count += 1
    save(surface, f"results/{name}.png")


class Drawer:
    def __init__(self, min_x, min_y, max_x, max_y, factor_x, factor_y):
        self.min_x = min_x
        self.min_y = min_y
        self.max_x = max_x
        self.max_y = max_y
        self.factor_x = factor_x
        self.factor_y = factor_y
        width = self.max_x - self.min_x
        height = self.max_y - self.min_y
        self.font = pygame.font.SysFont(None, 25)
        size = (width * self.factor_x + 100, height * self.factor_y + 100)
        print(f"Created surface with size: {size}")
        self.surface = pygame.Surface(size)

    def get_surface(self):
        return self.surface

    def get_draw_position(self, x, y):
        new_x = x - self.min_x
        new_y = y - self.min_y
        return new_x * self.factor_x + 50, new_y * self.factor_y + 50

    def draw_text(self, text, x, y):
        draw_pos = self.get_draw_position(x, y)
        img = self.font.render(text, True, GRAY)
        self.surface.blit(img, draw_pos)


def visualise_graph(graph):
    xs = [x.position.x for x in graph.nodes]
    ys = [x.position.y for x in graph.nodes]
    max_x = max(xs)
    max_y = max(ys)
    min_x = min(xs)
    min_y = min(ys)
    drawer = Drawer(min_x, min_y, max_x, max_y, 100, 100)
    surface = drawer.get_surface()
    surface.fill(BLACK)

    # Draw wires
    for wire in graph.wires:
        start = wire.output.parent.position.tuple()
        end = wire.input.parent.position.tuple()

        pygame.draw.line(surface, WHITE, drawer.get_draw_position(*start), drawer.get_draw_position(*end))

    # Draw nodes
    for node in graph.nodes:

        pygame.draw.circle(surface, WHITE, drawer.get_draw_position(*node.position.tuple()), 20)
        drawer.draw_text(node.label, *node.position.tuple())

    save(surface, f"graph.png")





