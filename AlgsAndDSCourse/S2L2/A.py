def isBST(a, r, n):
    if n <= 1:
        return True
    bst_array = [0 for _ in range(n)]
    in_order(a[r - 1], bst_array)
    if all(bst_array[i] < bst_array[i + 1] for i in range(n - 1)):
        return True
    else:
        return False

def in_order(root, bst_array):
    global ind
    if root[1] != -1:
        in_order(a[root[1] - 1], bst_array)
    bst_array[ind] = root[0]
    ind += 1
    if root[2] != -1:
        in_order(a[root[2] - 1], bst_array)


n = int(input())
a = [[int(i) for i in input().split()] for _ in range(n)]
r = int(input())
ind = 0

if isBST(a, r, n):
    print("YES")
else:
    print("NO")