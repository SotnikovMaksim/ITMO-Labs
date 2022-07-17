import math
from sys import stdin
import random


class BST:
    class Node:
        def __init__(self, value, left=None, right=None, parent=None):
            self.left = left
            self.right = right
            self.parent = parent
            self.value = value

    def __init__(self, root=None):
        self.root = root

    def insert(self, v, x):
        if v is None:
            return self.Node(x)
        elif x < v.value:
            v.left = self.insert(v.left, x)
            v.left.parent = v
        elif x > v.value:
            v.right = self.insert(v.right, x)
            v.right.parent = v
        return v

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
            self.delete(v, minim.value)
            cur.value = value
        else:
            if cur.left:
                temp = cur.left
            else:
                temp = cur.right
            if cur != v:
                if cur == parent.left:
                    parent.left = temp
                else:
                    parent.right = temp
            else:
                v = temp

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


    def next(self, v, x):
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


if __name__ == '__main__':
    tree = BST()

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
    tree = BST()
    tests_passed = 0
    commands = ["insert", "delete", "exists", "next", "prev"]
    border = 10 ** 9
    tAns = ""
    sAns = ""
    a = set()
    for i in range(5000):
        command = random.choice(commands)
        num = random.randint(-border, border)
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
            print(f"Wrong answer with '{command} {num}':\ntree answer: {tAns}\nset answer: {sAns}\n")
            break
        else:
            tests_passed += 1
    print(f"Tests passed: {tests_passed}")