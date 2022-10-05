import math


class Vec2:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def length(self):
        return math.sqrt(self.x ** 2 + self.y ** 2)

    def __add__(self, other: 'Vec2'):
        return Vec2(self.x + other.x, self.y + other.y)

    def __sub__(self, other: 'Vec2'):
        return Vec2(self.x - other.x, self.y - other.y)

    def __mul__(self, other: float):
        return Vec2(self.x * other, self.y * other)

    def normalize(self):
        return self * (1 / self.length())

    def tuple(self):
        return self.x, self.y

    def __neg__(self):
        return self * -1

    def __repr__(self):
        return f"Vec2({self.x},{self.y})"
