import math


def update(l, r, v, lx, rx, x):
    if r <= lx or rx <= l:
        return
    if l <= lx and rx <= r:
        a[x][1] = v
        a[x][0] = v
        return
    mid = (lx + rx + 1) // 2
    propagate(x)
    update(l, r, v, lx, mid, 2 * x + 1)
    update(l, r, v, mid, rx, 2 * x + 2)
    a[x][0] = min(a[2 * x + 1][0], a[2 * x + 2][0])


def propagate(x):
    if a[x][1] != -1:
        for child in [2 * x + 1, 2 * x + 2]:
            a[child][1] = a[x][1]
            a[child][0] = a[x][0]
        a[x][1] = -1


def minim(l, r, x, lx, rx):
    if r <= lx or rx <= l:
        return math.inf
    if l <= lx and rx <= r:
        return a[x][0]
    propagate(x)
    mid = (lx + rx + 1) // 2
    left = minim(l, r, 2 * x + 1, lx, mid)
    right = minim(l, r, 2 * x + 2, mid, rx)
    return min(left, right)


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
a = [[0, -1] for _ in range(size * 2 - 1)]


for i in range(m):
    req = [int(i) for i in input().split()]
    if req[0] == 1:
        update(req[1], req[2], req[3], 0, size, 0)
    else:
        print(minim(req[1], req[2], 0, 0, size))