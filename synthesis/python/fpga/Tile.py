from Node import Lut, DoubleLut
from fpga.Location import Location
from fpga.Nets import Net


class Tile:
    def __init__(self, grid, location):
        self.grid = grid
        self.location = location
        self.bot = DoubleLut()
        self.bot.tile = self
        self.bot.position = "TOP"
        self.right = DoubleLut()
        self.right.tile = self
        self.right.position = "RIGHT"
        self.setPortLocations()

    def setPortLocations(self):
        self.bot.inputs[0].location = self.location.alterType(0)
        self.bot.inputs[1].location = self.location.down().alterType(1)
        self.bot.outputs[0].location = self.location.alterType(1)
        self.bot.outputs[1].location = self.location.down().alterType(0)

        self.right.inputs[0].location = self.location.alterType(1)
        self.right.inputs[1].location = self.location.right().alterType(0)
        self.right.outputs[0].location = self.location.alterType(0)
        self.right.outputs[1].location = self.location.right().alterType(1)

    def getNet(self, location):
        assert location.x == self.location.x and location.y == self.location.y
        if location.kind == 0:
            return Net(
                inputs=[
                    self.bot.inputs[0],
                    self.grid.getTile(location.left()).right.inputs[1],
                ],
                outputs=[
                    self.right.outputs[0],
                    self.grid.getTile(location.top()).bot.outputs[1],
                ],
                location=location
            )
        elif location.kind == 1:
            return Net(
                inputs=[
                    self.right.inputs[0],
                    self.grid.getTile(location.top()).bot.inputs[1],
                ],
                outputs=[
                    self.bot.outputs[0],
                    self.grid.getTile(location.left()).right.outputs[1],
                ],
                location=location
            )
        else:
            raise Exception("UNKNOWN LOCATION TYPE")
