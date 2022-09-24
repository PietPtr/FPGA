

class Net:
    def __init__(self, inputs, outputs):
        self.inputs = inputs
        self.outputs = outputs
        self.assignment = None

    def assignInput(self, node):
        self.assignment = hash(node)

