from fpga.Location import Location
from fpga.Tile import Tile


class Grid:
    def __init__(self):
        self.blocks = {}
        self.nets = set()

    def __getitem__(self, location):
        if location not in self.blocks.keys():
            self.blocks[location] = Tile(self, location)
        return self.blocks[location]

    def addNet(self, net):
        self.nets.add(net)

    def get_next_input(self):
        location = Location(0, 0)
        while True:
            input_lut = self[location].luts["up2"]
            input_port = input_lut.inputs[0]
            net = self[location].getNet(input_port)
            if net.assignment is None:
                return input_port, net
            location = location.right()


