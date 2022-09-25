import pygame

pygame.init()
font = pygame.font.SysFont(None, 15)
count = 0


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
