import math


def set(l, r, v, lx, rx, x):
    if r <= lx or rx <= l:
        return
    if l <= lx and rx <= r:
        a[x][0] = v * (rx - lx)
        a[x][1] = v
        a[x][2] = 0
        return
    propagate(x, rx - lx)
    mid = (lx + rx + 1) // 2
    set(l, r, v, lx, mid, 2 * x + 1)
    set(l, r, v, mid, rx, 2 * x + 2)
    a[x][0] = a[2 * x + 1][0] + a[2 * x + 2][0] + a[2 * x + 1][2] * (mid - lx) + a[2 * x + 2][2] * (mid - lx)
    a[x][2] = 0

#  [sum, set, add]
def add(l, r, v, lx, rx, x):
    if r <= lx or rx <= l:
        return
    if rx - lx != 1 and l <= lx and rx <= r:
        propagate(x, rx - lx)
        a[x][2] += v
        return
    if rx - lx == 1 and l <= lx and rx <= r:
        a[x][2] += v
        return
    propagate(x, rx - lx)
    mid = (lx + rx + 1) // 2
    add(l, r, v, lx, mid, 2 * x + 1)
    add(l, r, v, mid, rx, 2 * x + 2)
    a[x][0] = a[2 * x + 1][0] + a[2 * x + 2][0] + a[2 * x + 1][2] * (mid - lx) + a[2 * x + 2][2] * (mid - lx)
    a[x][2] = 0

# [sum, set, add]
def propagate(x, length):
    if a[x][1] != -1:
        a[2 * x + 1][0] = a[x][1] * (length // 2)
        a[2 * x + 1][1] = a[x][1]
        a[2 * x + 1][2] = 0
        a[2 * x + 2][0] = a[x][1] * (length // 2)
        a[2 * x + 2][1] = a[x][1]
        a[2 * x + 2][2] = 0
        a[x][0] = length * a[x][1]
        a[x][1] = -1
    if a[x][2] != 0:
        a[x][0] += a[x][2] * length
        a[2 * x + 1][2] += a[x][2]
        a[2 * x + 2][2] += a[x][2]
        a[x][2] = 0


def sum(l, r, x, lx, rx):
    if r <= lx or rx <= l:
        return 0
    if rx - lx == 1 and l <= lx and rx <= r:
        return a[x][0] + a[x][2] * (rx - lx)
    if l <= lx and rx <= r:
        propagate(x, rx - lx)
        return a[x][0] + a[x][2] * (rx - lx)
    propagate(x, rx - lx)
    mid = (lx + rx + 1) // 2
    left = sum(l, r, 2 * x + 1, lx, mid)
    right = sum(l, r, 2 * x + 2, mid, rx)
    return left + right


def dump():
    for i in range(size):
        print(a[i + size - 1][1], end=' ')


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
a = [[0, -1, 0] for _ in range(size * 2 - 1)]

for i in range(m):
    req = [int(i) for i in input().split()]
    if req[0] == 1:
        set(req[1], req[2], req[3], 0, size, 0)
    elif req[0] == 2:
        add(req[1], req[2], req[3], 0, size, 0)
    else:
        print(sum(req[1], req[2], 0, 0, size))