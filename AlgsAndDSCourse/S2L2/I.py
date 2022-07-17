import random


class MoveToFront:
    class Node:
        def __init__(self, value, left=None, right=None):
            self.value = value
            self.left = left
            self.right = right
            self.size = 1
            self.reverse = False
            self.p = random.randint(-10 ** 5, 10 ** 5)

    def __init__(self, root=None):
        self.root = root

    def add(self, i, x):
        rootLeft, rootRight = self.split(self.root, i)
        new = self.Node(x)
        left = self.merge(rootLeft, new)
        self.root = self.merge(left, rootRight)
        self.recalc(self.root)

    def split(self, v, x):
        if v is None:
            return None, None
        if x <= self.size(v.left):
            sl, sr = self.split(v.left, x)
            v.left = sr
            self.recalc(v)
            return sl, v
        else:
            sl, sr = self.split(v.right, x - self.size(v.left) - 1)
            v.right = sl
            self.recalc(v)
            return v, sr

    def merge(self, left, right):
        if left is None:
            return right
        if right is None:
            return left

        if left.p > right.p:
            left.right = self.merge(left.right, right)
            self.recalc(left)
            return left
        else:
            right.left = self.merge(left, right.left)
            self.recalc(right)
            return right

    def recalc(self, v):
        v.size = self.size(v.left) + self.size(v.right) + 1

    def size(self, v):
        if v is None:
            return 0
        return v.size

    def moveTo(self, left, right):
        sl, sr = self.split(self.root, left - 1)
        middle, right = self.split(sr, right - left + 1)

        first = self.merge(sl, right)
        tree.root = self.merge(middle, first)

    def inOrder(self, v):
        if v is not None:
            self.inOrder(v.left)
            print(v.value, end=' ')
            self.inOrder(v.right)


if __name__ == '__main__':
    n, m = [int(i) for i in input().split()]
    tree = MoveToFront()
    for i in range(n):
        tree.add(i, i + 1)

    for i in range(m):
        l, r = [int(i) for i in input().split()]
        tree.moveTo(l, r)

    tree.inOrder(tree.root)