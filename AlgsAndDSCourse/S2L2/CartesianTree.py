import random


class Cartesian:
    class Node:
        def __init__(self, value, left=None, right=None):
            self.value = value
            self.left = left
            self.right = right
            self.p = random.randint(-10 ** 5, 10 ** 5)
            self.size = 1

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

    def delete(self, x):
        if self.root is None:
            return
        sl, sr = self.split(self.root, x)
        left, right = self.split(sr, 1)
        self.root = self.merge(sl, right)

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

    def inOrder(self, v):
        if v is not None:
            self.inOrder(v.left)
            print(v.value, end=' ')
            self.inOrder(v.right)


if __name__ == '__main__':
    n, m = [int(i) for i in input().split()]
    tree = Cartesian()
    a = [int(i) for i in input().split()]
    for i in range(len(a)):
        tree.add(i, a[i])

    for i in range(m):
        request = input().split()

        if request[0] == "add":
            tree.add(int(request[1]), int(request[2]))
        else:
            tree.delete(int(request[1]) - 1)

    print(tree.size(tree.root))
    tree.inOrder(tree.root)