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
        left = self.height(v.left)
        right = self.height(v.right)
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
        return v

    @staticmethod
    def height(v):
        if v is None:
            return 0
        return v.height

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
        return self.height(v.left) - self.height(v.right)

    @staticmethod
    def recalc(v):
        if v is None:
            return
        left, right = 0, 0
        if v.left:
            left = v.left.height
        if v.right:
            right = v.right.height

        v.height = max(left, right) + 1

    @staticmethod
    def rotateRight(v):
        child = v.left
        v.left = child.right
        child.right = v
        return child

    def bigRotateRight(self, v):
        v.left = self.rotateLeft(v.left)
        v = self.rotateRight(v)
        return v

    @staticmethod
    def rotateLeft(v):
        child = v.right
        v.right = child.left
        child.left = v
        return child

    def bigRotateLeft(self, v):
        v.right = self.rotateRight(v.right)
        v = self.rotateLeft(v)
        return v


if __name__ == '__main__':
    tree = AVL()

    for line in stdin:
        type, x = line.split()
        if type == "insert":
            tree.root = tree.insert(tree.root, int(x))
        elif type == "exists":
            res = tree.exists(tree.root, int(x))
            if res:
                print("true")
            else:
                print("false")
        elif type == "next":
            res = tree.next(tree.root, int(x))
            if res is None:
                print("none")
            else:
                print(res.value)
        elif type == "prev":
            res = tree.prev(tree.root, int(x))
            if res is None:
                print("none")
            else:
                print(res)
        elif type == "delete":
            tree.root = tree.delete(tree.root, int(x))


# DEBUG FUNCTION
def generator():
    tree = AVL()
    tests_passed = 0
    commands = ["insert", "delete", "exists", "next", "prev"]
    border = 10 ** 5
    tAns = ""
    sAns = ""
    a = set()
    used = set()
    passed = []
    for i in range(35):
        command = random.choice(commands)
        fromType = random.getrandbits(1)
        if len(used) != 0 and fromType:
            num = random.choice(tuple(used))
        else:
            num = random.randint(-border, border)
            used.add(num)
        if command == "insert":
            tree.root = tree.insert(tree.root, num)
            a.add(num)
        elif command == "exists":
            res = tree.exists(tree.root, num)
            if res:
                tAns = True
            else:
                tAns = False
            sAns = num in a
        elif command == "next":
            res = tree.next(tree.root, num)
            if res is None:
                tAns = "none"
            else:
                tAns = res.value
            try:
                sAns = min(filter(lambda x: x > num, a))
            except ValueError:
                sAns = "none"
        elif command == "prev":
            res = tree.prev(tree.root, num)
            if res is None:
                tAns = "none"
            else:
                tAns = res
            try:
                sAns = max(filter(lambda x: x < num, a))
            except ValueError:
                sAns = "none"
        elif command == "delete":
            tree.root = tree.delete(tree.root, num)
            a.discard(num)
        if sAns != tAns:
            passed.append(F"{command} {num}")
            print(f"Wrong answer with '{command} {num}':\ntree answer: {tAns}\nset answer: {sAns}\n")
            break
        else:
            tests_passed += 1
            passed.append(F"{command} {num}")
    for i in passed:
        print(i)
    print(f"Tests passed: {tests_passed}\n")



# DEBUG FUNCTION
def dump(v):
    if v is not None:
        dump(v.left)
        print(v.value, end=' ')
        dump(v.right)