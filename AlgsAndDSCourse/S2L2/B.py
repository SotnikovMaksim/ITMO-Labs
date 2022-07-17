def toBST(a, n):
    if n == 0:
        return []
    if n == 1:
        return [[a[0], -1, -1]]
    bst = []
    ind = 2
    for i in reversed(range(1, len(a))):
        bst.append([a[i], ind, -1])
        ind += 1
    bst.append([a[0], -1, -1])
    return bst


n = int(input())
a = [int(i) for i in input().split()]
a.sort()

bst = toBST(a, n)
print(n)
for v in bst:
    print(*v)
print(1)