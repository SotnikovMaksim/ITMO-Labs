import heapq
import random
from sys import stdin


class AVL:
    class Node:
        def __init__(self, value, left=None, right=None, parent=None, height=1):
            self.left = left
            self.right = right
            self.parent = parent
            self.value = value
            self.height = height
            self.size = 1

    def __init__(self, root=None):
        self.root = root

    def insert(self, v, x):
        if v is None:
            return self.Node(x)
        if x < v.value:
            v.left = self.insert(v.left, x)
            self.recalc(v)
            v.left.parent = v
        elif x > v.value:
            v.right = self.insert(v.right, x)
            self.recalc(v)
            v.right.parent = v
        v = self.letsRotate(v)
        return v

    def letsRotate(self, v):
        if v is None:
            return None
        self.recalc(v)
        left = self.getHeight(v.left)
        right = self.getHeight(v.right)
        if right > left + 1:
            if v.right is not None:
                if v.right.right is not None:
                    v = self.rotateLeft(v)
                elif v.right.left is not None:
                    v = self.bigRotateLeft(v)
        elif left > right + 1:
            if v.left is not None:
                if v.left.left is not None:
                    v = self.rotateRight(v)
                elif v.left.right is not None:
                    v = self.bigRotateRight(v)

        self.recalc(v)
        return v

    def getHeight(self, v):
        if v is None:
            return 0
        return v.height

    def size(self, v):
        if v is None:
            return 0
        return v.size

    def delete(self, v, x):
        if v is None:
            return None
        parent = None
        cur = v
        while cur and cur.value != x:
            parent = cur
            if x < cur.value:
                cur = cur.left
            else:
                cur = cur.right
        if cur is None:
            return v
        if cur.left is None and cur.right is None:
            if cur != v:
                if parent.left == cur:
                    parent.left = None
                else:
                    parent.right = None
                self.recalc(parent)
            else:
                v = None
        elif cur.left and cur.right:
            minim = self.getMostLeft(cur.right)
            value = minim.value
            v = self.delete(v, minim.value)
            cur.value = value
        else:
            if cur.left:
                temp = cur.left
            else:
                temp = cur.right
            self.recalc(temp)
            if cur != v:
                if cur == parent.left:
                    parent.left = temp
                else:
                    parent.right = temp
                self.recalc(parent)
            else:
                v = temp
        v = self.letsRotate(v)
        return v

    def exists(self, v, x):
        if v is None:
            return False
        if v.value == x:
            return True
        if x < v.value:
            return self.exists(v.left, x)
        return self.exists(v.right, x)

    def find(self, v, x):
        if v is None:
            return None
        if v.value == x:
            return v
        if x < v.value:
            return self.find(v.left, x)
        return self.find(v.right, x)

    @staticmethod
    def next(v, x):
        cur = v
        compared = None
        while cur:
            if x >= cur.value:
                cur = cur.right
            else:
                compared = cur
                cur = cur.left
        return compared

    def getMostLeft(self, v):
        if v is None:
            return None
        if v.left is None:
            return v
        return self.getMostLeft(v.left)

    def getMostRight(self, v):
        if v is None:
            return None
        if v.right is None:
            return v
        return self.getMostRight(v.right)

    def prev(self, v, x):
        if v is None:
            return None
        if x > v.value:
            if v.right is not None:
                res = self.prev(v.right, x)
                if res is None:
                    return v.value
                return res
            else:
                return v.value
        else:
            return self.prev(v.left, x)

    def diff(self, v):
        if v is None:
            return 0
        return self.getHeight(v.left) - self.getHeight(v.right)

    def recalc(self, v):
        if v is None:
            return
        v.height = max(self.getHeight(v.left), self.getHeight(v.right)) + 1
        v.size = self.size(v.left) + self.size(v.right) + 1
        self.recalc(v.parent)

    def kthLargest(self, v, k):
        if v is None:
            return "fuck it"
        if self.size(v.right) >= k:
            return self.kthLargest(v.right, k)
        elif self.size(v.right) + 1 == k:
            return v.value
        else:
            return self.kthLargest(v.left, k - self.size(v.right) - 1)

    def rotateRight(self, v):
        child = v.left
        v.left = child.right
        if v.left:
            v.left.parent = v
        child.right = v
        child.parent = v.parent
        child.right.parent = child
        self.recalc(v)
        return child

    def bigRotateRight(self, v):
        v.left = self.rotateLeft(v.left)
        self.recalc(v.left)
        v = self.rotateRight(v)
        self.recalc(v)
        return v

    def rotateLeft(self, v):
        child = v.right
        v.right = child.left
        if v.right:
            v.right.parent = v
        child.left = v
        child.parent = v.parent
        child.left.parent = child
        self.recalc(v)
        return child

    def bigRotateLeft(self, v):
        v.right = self.rotateRight(v.right)
        self.recalc(v.right)
        v = self.rotateLeft(v)
        self.recalc(v)
        return v


def dump(v):
    order(v)
    print()


def order(v):
    if v is not None:
        order(v.left)
        print(v.value, end=' ')
        order(v.right)


def find_kth_largest(ints, k):
    if not ints or len(ints) < k:
        exit(-1)
    pq = ints[0:k]
    heapq.heapify(pq)
    for i in range(k, len(ints)):
        if ints[i] > pq[0]:
            heapq.heapreplace(pq, ints[i])
    return pq[0]


if __name__ == '__main__':
    tree = AVL()
    requests_count = int(input())
    k = 0
    for i in range(requests_count):
        type, x = input().split()
        if type == "1":
            tree.root = tree.insert(tree.root, int(x))
        elif type == "-1":
            tree.root = tree.delete(tree.root, int(x))
        elif type == "0":
            print(tree.kthLargest(tree.root, int(x)))


# DEBUG FUNCTION
def generator():
    tests_passed = 0
    commands = ["1", "-1", "0"]
    border = 10 ** 5
    command = ""
    k = 100
    tAns = ""
    sAns = ""
    n = 100000
    passed = []
    success = False
    for i in range(50000):
        tree = AVL()
        a = set()
        used = set()
        passed = []
        tests_passed = 0
        for i in range(n):
            command = random.choice(commands)
            if command == "0" and len(a) == 0:
                continue
            fromType = random.getrandbits(1)
            if len(used) != 0 and fromType:
                num = random.choice(tuple(used))
            else:
                if command == "0":
                    continue
                num = random.randint(-border, border)
                used.add(num)
            if command == "1":
                tree.root = tree.insert(tree.root, num)
                a.add(num)
            elif command == "-1":
                tree.root = tree.delete(tree.root, num)
                a.discard(num)
            elif command == "0":
                k = random.randint(1, len(a))
                tAns = tree.kthLargest(tree.root, k)
                sAns = find_kth_largest(list(a), k)
            if sAns != tAns:
                success = True
                if command == "0":
                    passed.append(F"{command} {k}")
                else:
                    passed.append(F"{command} {num}")
                break
            else:
                if command == "0":
                    passed.append(F"{command} {k}")
                else:
                    passed.append(F"{command} {num}")
                tests_passed += 1
        if success:
            break
    if success:
        print(f"Wrong answer with '{command} {k}':\ntree answer: {tAns}\nset answer: {sAns}\n")
        print(f"Tests passed: {tests_passed}\n")
        for i in passed:
            print(i)
