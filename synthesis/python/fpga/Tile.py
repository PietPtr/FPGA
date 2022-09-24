from Node import Lut
from fpga.Nets import Net

lut_names = [
    "down1",
    "down2",
    "right1",
    "left2",
    "up2",
    "up1",
    "right2",
    "left2"
]


class Tile:
    def __init__(self, grid, location):
        self.grid = grid
        self.location = location
        self.net_assignments = dict()
        self.luts = {n: Lut(n) for n in lut_names}
        self.port_nets = dict()
        self.nets = dict()
        self._init_connections()

    def _init_connections(self):
        self.nets = {
            "left_down": Net(
                inputs=[
                    (self.location, "down2", 1),
                    (self.location.left(), "left1", 1),
                    (self.location.bottom(), "down1", 0),
                    (self.location.left(), "left2", 0)
                ],
                outputs=[
                    (self.location, "left2", 0),
                    (self.location, "down1", 0)
                ]
            ),
            "left_top": Net(
                inputs=[
                    (self.location, "down1", 1),
                    (self.location, "right1", 1),
                    (self.location, "down2", 0),
                    (self.location, "right2", 0)
                ],
                outputs=[
                    (self.location.top(), "down2", 0),
                    (self.location.left(), "right2", 0)
                ]
            ),
            "right_down": Net(
                inputs=[
                    (self.location, "up2", 1),
                    (self.location.top(), "up1", 0),
                    (self.location, "left2", 1),
                    (self.location.left(), "left1", 0)
                ],
                outputs=[
                    (self.location, "left1", 0),
                    (self.location, "up1", 0)
                ]
            ),
            "right_top": Net(
                inputs=[
                    (self.location, "right2", 1),
                    (self.location.right(), "right1", 0),
                    (self.location.top(), "up1", 1),
                    (self.location.top(), "up2", 0)
                ],
                outputs=[
                    (self.location, "right1", 0),
                    (self.location, "up2", 0)
                ]
            )
        }

        for name, lut in self.luts.items():
            if name == "down1":
                self.port_nets[lut.inputs[0]] = (self.location.top(), "left_down")
                self.port_nets[lut.inputs[1]] = (self.location, "left_top")
                self.port_nets[lut.outputs[0]] = (self.location, "left_down")
            elif name == "down2":
                self.port_nets[lut.inputs[0]] = (self.location, "left_top")
                self.port_nets[lut.inputs[1]] = (self.location, "left_down")
                self.port_nets[lut.outputs[0]] = (self.location.bottom(), "left_top")
            elif name == "right1":
                self.port_nets[lut.inputs[0]] = (self.location.left(), "right_top")
                self.port_nets[lut.inputs[1]] = (self.location, "left_top")
                self.port_nets[lut.outputs[0]] = (self.location, "right_top")
            elif name == "left2":
                self.port_nets[lut.inputs[0]] = (self.location.right(), "left_down")
                self.port_nets[lut.inputs[1]] = (self.location, "right_down")
                self.port_nets[lut.outputs[0]] = (self.location, "left_down")
            elif name == "up2":
                self.port_nets[lut.inputs[0]] = (self.location.bottom(), "right_top")
                self.port_nets[lut.inputs[1]] = (self.location, "right_down")
                self.port_nets[lut.outputs[0]] = (self.location, "right_top")
            elif name == "up1":
                self.port_nets[lut.inputs[0]] = (self.location.bottom(), "right_down")
                self.port_nets[lut.inputs[1]] = (self.location.bottom(), "right_top")
                self.port_nets[lut.outputs[0]] = (self.location, "right_down")
            elif name == "right2":
                self.port_nets[lut.inputs[0]] = (self.location, "left_top")
                self.port_nets[lut.inputs[1]] = (self.location, "right_top")
                self.port_nets[lut.outputs[0]] = (self.location.right(), "left_top")
            elif name == "left1":
                self.port_nets[lut.inputs[0]] = (self.location.right(), "right_down")
                self.port_nets[lut.inputs[1]] = (self.location.right(), "left_down")
                self.port_nets[lut.outputs[0]] = (self.location, "right_down")

    def getInputNet(self):
        port = self.luts["up2"].inputs[0]
        return self.getNet(port)

    def getNet(self, port):
        location, net_name = self.port_nets[port]
        return self.grid[location].nets[net_name]
