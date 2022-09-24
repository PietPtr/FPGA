

class Location:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def left(self):
        return Location(self.x - 1, self.y)

    def right(self):
        return Location(self.x + 1, self.y)

    def top(self):
        return Location(self.x, self.y - 1)

    def bottom(self):
        return Location(self.x, self.y + 1)

    def __repr__(self):
        return f"Location({self.x}, {self.y}: {self.__hash__()})"

    def __hash__(self):
        return hash(f"{self.x}-{self.y}")

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y