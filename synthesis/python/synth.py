from Node import Input, Output, Lut, Register, Node
from fpga.Grid import Grid


class Synthesizer:
    def __init__(self, graph):
        self.graph = graph
        self.grid = Grid()

    def synth(self):
        placed = set()
        while len(placed) != len(self.graph.nodes): # Not all units are placed yet.
            node = [k for k, v in self.graph.dependencies.items() if v.issubset(placed) and k not in placed][0]
            self.place(node)
            placed.add(node)

    def place(self, node):
        if isinstance(node, Input):
            self.place_input(node)

        if isinstance(node, Output):
            self.place_output(node)

        if isinstance(node, Lut):
            self.place_lut(node)

        if isinstance(node, Register):
            self.place_register(node)

    def place_input(self, node):
        # Find a free net to map input to.
        port, net = self.grid.get_next_input()
        net.assignInput(node)
        self.grid.addNet(net)

    def place_output(self, node):
        pass

    def place_lut(self, node: Lut):
        net0 = node.inputs[0]
        net1 = node.inputs[1]




        pass

    def place_register(self, node):
        pass

