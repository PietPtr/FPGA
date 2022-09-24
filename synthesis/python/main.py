from Graph import Graph
from Node import Input, Output, Lut, Register, State
from Wire import Wire
from synth import Synthesizer

nodes = {
    "A": Input(label="A"),
    "B": Input(label="B"),
    "S": Output(label="S"),
    "C": Output(label="C"),
    "XOR": Lut(label="XOR", state=State.XOR),
    "AND": Lut(label="AND", state=State.AND)
}

wires = [
    Wire(nodes["A"].outputs[0], nodes["XOR"].inputs[0]),
    Wire(nodes["B"].outputs[0], nodes["XOR"].inputs[1]),
    Wire(nodes["A"].outputs[0], nodes["AND"].inputs[0]),
    Wire(nodes["B"].outputs[0], nodes["AND"].inputs[1]),
    Wire(nodes["XOR"].outputs[0], nodes["S"].inputs[0]),
    Wire(nodes["AND"].outputs[0], nodes["C"].inputs[0])
]

graph = Graph(nodes.values(), wires)
synth = Synthesizer(graph)
synth.synth()

