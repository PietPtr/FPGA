from enum import Enum


class State(Enum):
    DEFAULT = 0b0000
    OR = 0b1110
    AND = 0b1000
    XOR = 0b0110
    ROUTE_0 = 0b0101
    ROUTE_1 = 0b1010

    # Switches the 2 inputs
    def inverse(self):
        value = self.value
        static = value & 0b1001
        result = static | ((value & 0b0100) >> 1) | ((value & 0b0010) << 1)
        return State(result)


class Port:
    def __init__(self, parent):
        self.parent = parent
        self.wires = []
        self.output = None

    def addWire(self, wire):
        self.wires.append(wire)


class Node:
    def __init__(self, label):
        self.label = label
        self.inputs = []
        self.outputs = []
        pass

    def __repr__(self):
        return super().__repr__() + "-" + self.label


class Lut(Node):
    def __init__(self, label="", state=State.DEFAULT):
        super().__init__(label)
        self.state = state
        self.inputs = [Port(self), Port(self)]
        self.outputs = [Port(self)]
        pass

    def getTag(self):
        return f"({self.inputs[0].output.parent.getTag()}:{self.inputs[1].output.parent.getTag()}:{self.state})"


class DoubleLut(Node):
    def __init__(self, label="", states=None):
        super().__init__(label)
        if states is None:
            states = [State.DEFAULT, State.DEFAULT]
        self.states = states
        self.inputs = [Port(self), Port(self)]
        self.outputs = [Port(self), Port(self)]


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

    def getTag(self):
        return self.label


class Output(Node):
    def __init__(self, label=""):
        super().__init__(label)
        self.inputs = [Port(self)]
        self.outputs = []

    def getTag(self):
        return self.inputs[0].output.parent.getTag()
