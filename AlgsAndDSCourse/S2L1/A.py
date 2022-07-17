import math


def set(i, v):
    x = i + size - 1
    a[x] = v
    x = (x + 1) // 2 - 1
    while x >= 0:
        a[x] = a[2 * x + 1] + a[2 * x + 2]
        x = (x + 1) // 2 - 1


def summ(l, r, x, lx, rx):
    if l >= rx or lx >= r:
        return 0
    if lx >= l and rx < r:
        return a[x]
    if rx - lx <= 1:
        return a[lx + size - 1]
    mid = (lx + rx) // 2
    ans = 0
    ans += summ(l, r, 2 * x + 1, lx, mid)
    ans += summ(l, r, 2 * x + 2, mid, rx)
    return ans


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
s = [int(i) for i in input().split()]
a = [0 for _ in range(size * 2 - 1)]
for i in range(n):
    set(i, s[i])

for i in range(m):
    req = [int(j) for j in input().split()]
    if req[0] == 1:
        set(req[1], req[2])
    else:
        print(summ(req[1], req[2], 0, 0, size))