from Node import Input, Output, Register


class Graph:
    def __init__(self, nodes, wires):
        self.nodes = nodes
        self.wires = wires

        self.inputs = [x for x in self.nodes if isinstance(x, Input)]
        self.outputs = [x for x in self.nodes if isinstance(x, Output)]

        self._wire_ports()
        self._calculate_dependencies()

    def _wire_ports(self):
        for wire in self.wires:
            wire.output.addWire(wire)
            wire.input.addWire(wire)

    def _calculate_dependencies(self):
        self.dependencies = {x: set() for x in self.inputs}

        while True:
            new_dependencies = {k: v for k, v in self.dependencies.items()}
            for wire in self.wires:
                o = wire.output.parent
                i = wire.input.parent
                if isinstance(i, Register):
                    continue

                if o in new_dependencies:
                    new_dependencies[o] = new_dependencies[o].union(new_dependencies[i])
                else:
                    new_dependencies[o] = set().union(new_dependencies[i])
                new_dependencies[o].add(i)
            if new_dependencies == self.dependencies:
                return
            self.dependencies = new_dependencies
