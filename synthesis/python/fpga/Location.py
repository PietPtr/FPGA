

class Location:
    def __init__(self, x, y, kind):
        self.x = x
        self.y = y
        self.kind = kind

    def left(self):
        return Location(self.x - 1, self.y, self.kind)

    def right(self):
        return Location(self.x + 1, self.y, self.kind)

    def top(self, amount=1):
        return Location(self.x, self.y - amount, self.kind)

    def down(self, amount=1):
        return Location(self.x, self.y + amount, self.kind)

    def alterType(self, kind):
        return Location(self.x, self.y, kind)

    def distance(self, other):
        manhattan = abs(self.x - other.x) + abs(self.y - other.y)
        if manhattan % 2 == abs(self.kind - other.kind):
            return manhattan
        else: # We have to make an extra jump to sync the net kinds of the tiles
            return manhattan + 1

    def __repr__(self):
        return f"Location({self.x},{self.y}:{self.kind})"

    def __hash__(self):
        return hash(f"{self.x}-{self.y}-{self.kind}")

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y and self.kind == other.kind

    def __lt__(self, other):
        return self.distance(Location(0, 0, 0)) < other.distance(Location(0, 0, 0))
