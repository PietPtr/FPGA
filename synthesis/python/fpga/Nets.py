

class Net:
    def __init__(self, inputs, outputs, location):
        self.inputs = inputs
        self.outputs = outputs
        self.assignment = None
        self.location = location

    def assignInput(self, node):
        self.assignment = hash(node)

