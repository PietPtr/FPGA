import random

from Node import Input, Output, Lut, Register, Node, State
from fpga.Grid import Grid
from copy import deepcopy

from fpga.Location import Location
from render.visualise import visualise
from search import astar, astar2


class Synthesizer:
    def __init__(self, graph):
        self.graph = graph

    def synth(self):
        order = self.placement_order()
        print(f"Placement order: {order}")
        self.place_recursive(order, Grid())

    def placement_order(self):
        order = []
        while len(order) != len(self.graph.nodes):  # Not all units are placed yet.
            node = [k for k, v in self.graph.dependencies.items() if v.issubset(order) and k not in order][0]
            order.append(node)
        return order

    def place_recursive(self, leftover, grid):
        if len(leftover) == 0:
            return grid

        current_node = leftover[0]
        print(f"Placing node: {current_node}")

        # try 5 placements to see if one succeeds
        tries = []
        while len(tries) < 5:
            new_grid = deepcopy(grid)
            if self.place(current_node, new_grid, tries):
                result = self.place_recursive(leftover[1:], new_grid)
                if result:
                    return result
            else:
                return False
        return False

    def place(self, node, grid, tries):
        if isinstance(node, Input):
            return self.place_input(node, grid, tries)

        if isinstance(node, Output):
            return self.place_output(node, grid, tries)

        if isinstance(node, Lut):
            return self.place_lut(node, grid, tries)

        if isinstance(node, Register):
            return self.place_register(node, grid, tries)

    def place_input(self, node, grid, tries):
        # Find a free net to map input to.
        location = Location(1, 0, 0)
        while True:
            net = grid.getNet(location)
            if net.assignment is None:
                if len(tries) > 0:
                    tries.append(location)
                    location = location.right()
                    continue
                else:
                    net.assignment = node.label
                    print(f"assigning {node.label} to {location}")
                    grid.signals[node.label] = [location]
                    print(f"grid signals: {grid.signals}")
                    return True
            else:
                location = location.right()
        return False

    def place_output(self, node, grid, tries):
        # Route to free output net by first finding one
        location = Location(1, 0, 1)
        while True:
            net = grid.getNet(location)
            if net.assignment is None:
                if len(tries) > 0:
                    tries.append(location)
                    location = location.right()
                    continue
                else:
                    return self.route_to_output(node, grid, location)
            else:
                location = location.right()
        return False

    def route_to_output(self, node, grid, location):
        signal = node.getTag()
        print(f"looking for {signal} in {grid.signals}")
        existing_nets = grid.signals[signal]
        start = random.choice(existing_nets)

        path = astar2(grid, grid.getNet(location), grid.getNet(start))
        if not path:
            print("COULD NOT FIND PATH TO OUTPUT")
            return False

        for n in path[1:]:
            grid.link(start, n.location)
            start = n.location

    def place_lut(self, node: Lut, grid, tries):
        print(f"RUNNING PLACEMENT WITH BLACKLIST: {tries}")
        success, nodes = self.place_lut_part(node, grid, tries)
        if success:
            tries.extend(nodes)
            print(f"EXTEND TRIES: {tries} with {nodes}")
            return True
        else:
            return False

    def place_lut_part(self, node, grid, blacklist):
        visualise(grid)
        print("PLACING LUT")
        input_node0 = node.inputs[0].output.parent
        input_node1 = node.inputs[1].output.parent
        node0_tag = input_node0.getTag()
        node1_tag = input_node1.getTag()

        net0_locations = grid.signals[node0_tag]
        net1_locations = grid.signals[node1_tag]

        print(f"grid_signals: {grid.signals}")
        print(f"node0_tag: {node0_tag}, node1_tag: {node1_tag}")

        net0_start = random.choice(net0_locations)
        net1_start = random.choice(net1_locations)

        print(f"net0_start, {net0_start}, net1_start: {net1_start}")
        print(f"BLACKLIST before loop: {blacklist}")

        while True:
            path0 = astar2(grid, goal=grid.getNet(net1_start), start=grid.getNet(net0_start))
            path1 = astar2(grid, goal=grid.getNet(net0_start), start=grid.getNet(net1_start))
            if not path0 or not path1:
                return False, []
            print(path0, path1)
            if len(path0) <= len(path1):
                if len(path0) == 1:
                    break
                print("select path 0")
                grid.link(path0[0].location, path0[1].location)
                net0_start = path0[1].location
            else:
                if len(path1) == 1:
                    break
                print("select path 1")
                grid.link(path1[0].location, path1[1].location)
                net1_start = path1[1].location


        visualise(grid)
        # Find the lut connecting the two.
        neighbour_luts_0 = set([x.parent for x in grid.getNet(net0_start).inputs])
        neighbour_luts_1 = set([x.parent for x in grid.getNet(net1_start).inputs])
        print(neighbour_luts_0, neighbour_luts_1)
        print("locations of luts", [(x.tile.location, x.position) for x in neighbour_luts_0],
              [(x.tile.location, x.position) for x in neighbour_luts_1])
        print(net0_start, net1_start)
        lut = neighbour_luts_0.intersection(neighbour_luts_1).pop()
        if lut.states != [State.DEFAULT, State.DEFAULT]:
            visualise(grid)
            print(f"LUT NOT IN DEFAULT STATE: {lut.tile.location} {lut.position}")
            # raise Exception(f"LUT NOT IN DEFAULT STATE: {lut.tile.location} {lut.position}")
            return False, []
        if grid.getNet(lut.outputs[0].location).assignment is None:
            # Map the lut to this lut location with this output
            output_side = 0
            grid.getNet(lut.outputs[0].location).assignment = node.getTag()
            grid.add_or_create_signal(node.getTag(), [lut.outputs[0].location])

        elif grid.getNet(lut.outputs[1].location).assignment is None:
            output_side = 1
            grid.getNet(lut.outputs[1].location).assignment = node.getTag()
            grid.add_or_create_signal(node.getTag(), [lut.outputs[1].location])
        else:
            return False, []
            # raise Exception("Both sides of the lut output are assigned already")

        if lut.inputs[0].location == net0_start:
            lut.states[output_side] = node.state
        else:
            lut.states[output_side] = node.state.inverse()

        visualise(grid)
        print("LUT PLACED at ", lut.tile.location, lut.position)
        return True, [net0_start, net1_start]

    def place_register(self, node, grid, skip):
        return True
