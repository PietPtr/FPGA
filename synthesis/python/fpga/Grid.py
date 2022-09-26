from Node import State
from fpga.Location import Location
from fpga.Nets import Net
from fpga.Tile import Tile


class Grid:
    def __init__(self):
        self.blocks = {}
        self.nets = {}
        self.signals = {}

    def getTile(self, location):
        location = location.alterType(0)
        if location not in self.blocks.keys():
            self.blocks[location] = Tile(self, location)
        return self.blocks[location]

    def getNet(self, location):
        if location not in self.nets.keys():
            net = self.getTile(location).getNet(location)
            self.nets[location] = net
        return self.nets[location]

    def connections(self, location):
        neighbours = set()
        net = self.getNet(location)
        for port in net.inputs:
            lut = port.parent
            neighbours.add(lut.outputs[0].location)
            neighbours.add(lut.outputs[1].location)
        return neighbours

    def link(self, net_drive, net_out):
        luts_drive = set([x.parent for x in self.getNet(net_drive).inputs])
        luts_out = set([x.parent for x in self.getNet(net_out).outputs])
        print("LINKING", net_drive, net_out)
        print(luts_drive, luts_out)
        print("locations of luts", [(x.tile.location, x.position) for x in luts_drive], [(x.tile.location, x.position) for x in luts_out])
        lut = luts_drive.intersection(luts_out).pop()
        print("LOCATION OF SINGLE LUT THAT WE ARE GOING TO EDIT", lut.tile.location, lut.position)

        input_num = 0 if lut.inputs[0].location == net_drive else 1
        output_num = 0 if lut.outputs[0].location == net_out else 1

        if input_num == 0:
            lut.states[output_num] = State.ROUTE_0
        else:
            lut.states[output_num] = State.ROUTE_1

        assert self.getNet(net_out).assignment is None or self.getNet(net_out).assignment == self.getNet(net_drive).assignment
        self.getNet(net_out).assignment = self.getNet(net_drive).assignment
        self.add_or_create_signal(self.getNet(net_drive).assignment, [net_out])

    def find_shared_lut_inputs(self, loc0, loc1):
        luts0 = set([x.parent for x in self.getNet(loc0).inputs])
        luts1 = set([x.parent for x in self.getNet(loc1).inputs])
        return luts0.intersection(luts1)

    def add_or_create_signal(self, signal, locations):
        if signal not in self.signals:
            self.signals[signal] = []
        self.signals[signal].extend(locations)

