import random


class Tree:
    class Node:
        def __init__(self, value, left=None, right=None, parent=None):
            self.value = value
            self.left = left
            self.right = right
            self.parent = parent
            self.sum = value
            self.p = random.randint(1, 10 ** 9)

    def __init__(self, root=None):
        self.root = root

    def merge(self, left, right):
        if left is None:
            return right
        if right is None:
            return left
        if left.p <= right.p:
            left.right = self.merge(left.right, right)
            self.recalc(left)
            return left
        else:
            right.left = self.merge(left, right.left)
            self.recalc(right)
            return right

    def getValue(self, v):
        if v is None:
            return 0
        return v.value

    def split(self, v, x):
        if v is None:
            return None, None
        if x < self.getValue(v):
            sl, v.left = self.split(v.left, x)
            sr = v
            self.recalc(sr)
        else:
            v.right, sr = self.split(v.right, x)
            sl = v
            self.recalc(sl)
        return sl, sr

    def add(self, v, x):
        if v is None:
            v = self.Node(x)
            v.sum = v.value
            return v
        if self.find(v, x):
            return v
        added = self.Node(x)
        sl, sr = self.split(v, x)
        v = self.merge(self.merge(sl, added), sr)
        return v

    def recalc(self, v):
        if v.right and v.left:
            v.sum = v.left.sum + v.right.sum + self.getValue(v)
        elif v.right:
            v.sum = self.getValue(v) + v.right.sum
        elif v.left:
            v.sum = self.getValue(v) + v.left.sum
        else:
            v.sum = self.getValue(v)

    def sum(self, v):
        if v is None:
            return 0
        return v.sum

    def find(self, v, x):
        if v is None:
            return False
        if v.value == x:
            return True
        if x < v.value:
            return self.find(v.left, x)
        return self.find(v.right, x)

    def countSum(self, v, left, right):
        sl1, sr1 = self.split(v, right)
        sl2, sr2 = self.split(sl1, left - 1)
        res = self.sum(sr2)
        v = self.merge(self.merge(sl2, sr2), sr1)
        return res, v


if __name__ == '__main__':
    tree = Tree()
    prev = ''
    y = 0

    requests_count = int(input())
    for i in range(requests_count):
        request = input().split()
        if request[0] == "+":
            if prev == "?":
                tree.root = tree.add(tree.root, (int(request[1]) + y) % (10 ** 9))
            else:
                tree.root = tree.add(tree.root, int(request[1]))
        elif request[0] == "?":
            y, tree.root = tree.countSum(tree.root, int(request[1]), int(request[2]))
            print(y)
        prev = request[0]