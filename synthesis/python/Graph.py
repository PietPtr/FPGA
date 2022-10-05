import math
import random

from Node import Input, Output, Register
from Vec2 import Vec2
from render.visualise import visualise_graph


class Graph:
    def __init__(self, nodes, wires):
        self.nodes = nodes
        self.wires = wires

        self.inputs = [x for x in self.nodes if isinstance(x, Input)]
        self.outputs = [x for x in self.nodes if isinstance(x, Output)]

        self._wire_ports()
        self._calculate_dependencies()
        self._simulate_springs()

    def _wire_ports(self):
        for wire in self.wires:
            wire.output.addWire(wire)
            wire.input.addWire(wire)
            wire.input.output = wire.output

    def _simulate_springs(self):
        # Init positions at random
        force_factor = 0.05
        node_force_factor = 0.01

        for node in self.nodes:
            node.position = Vec2(random.random(), random.random())
            node.velocity = Vec2(0.0, 0.0)
            node.force = Vec2(0.0, 0.0)

        for i in range(50):
            for node in self.nodes:
                node.force = Vec2(0.0, 0.0)

            # Pull on wires.
            for wire in self.wires:
                input_to_output = wire.output.parent.position - wire.input.parent.position
                input_force = input_to_output.normalize() * force_factor * (input_to_output.length() ** 2)

                wire.input.parent.force += input_force
                wire.output.parent.force += -input_force
                print(f"input: {wire.input.parent.position} output: {wire.output.parent.position} between: {input_to_output} force: {input_force}")

            # Push away from other nodes.
            for node in self.nodes:
                for other in self.nodes:
                    if other == node:
                        continue
                    other_to_node = node.position - other.position
                    print(f"other_to_node: {other_to_node}")
                    force = other_to_node.normalize() * node_force_factor *  (1.0 / math.sqrt(other_to_node.length()))
                    print(f"Force: {force}")
                    node.force += force

            for node in self.nodes:
                node.velocity += node.force

            for node in self.nodes:  # Drag
                node.velocity *= 0.9
                print(f"Velocity: {node.velocity}")
            for node in self.nodes:
                node.position += node.velocity

        visualise_graph(self)
        raise Exception("STOP")

    def _calculate_dependencies(self):
        self.dependencies = {x: set() for x in self.inputs}

        while True:
            new_dependencies = {k: v for k, v in self.dependencies.items()}
            for wire in self.wires:
                o = wire.output.parent
                i = wire.input.parent
                if isinstance(i, Register):
                    continue

                if o in new_dependencies and i in new_dependencies:
                    new_dependencies[i] = new_dependencies[i].union(new_dependencies[o])
                elif o in new_dependencies:
                    new_dependencies[i] = set().union(new_dependencies[o])
                else:
                    new_dependencies[i] = set()
                new_dependencies[i].add(o)
            if new_dependencies == self.dependencies:
                return
            self.dependencies = new_dependencies
