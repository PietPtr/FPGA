from enum import Enum


class State(Enum):
    DEFAULT = 0b0000
    OR = 0b1110
    AND = 0b1000
    XOR = 0b0110


class Port:
    def __init__(self, parent):
        self.parent = parent
        self.wires = []

    def addWire(self, wire):
        self.wires.append(wire)


class Node:
    def __init__(self, label):
        self.label = label
        self.inputs = []
        self.outputs = []
        pass


class Lut(Node):
    def __init__(self, label="", state=State.DEFAULT):
        super().__init__(label)
        self.state = state
        self.inputs = [Port(self), Port(self)]
        self.outputs = [Port(self)]
        pass


class Register(Node):
    def __init__(self, label=""):
        super().__init__(label)
        self.inputs = [Port(self)]
        self.outputs = [Port(self)]
        pass


class Input(Node):
    def __init__(self, label=""):
        super().__init__(label)
        self.inputs = []
        self.outputs = [Port(self)]


class Output(Node):
    def __init__(self, label=""):
        super().__init__(label)
        self.inputs = [Port(self)]
        self.outputs = []
