import math


def update(i, v, l, r, x):
    if r - l == 1:
        a[x] = v
    else:
        mid = (r + l + 1) // 2
        if i < mid:
            update(i, v, l, mid, 2 * x + 1)
        else:
            update(i, v, mid, r, 2 * x + 2)
        a[x] = max(a[2 * x + 1], a[2 * x + 2])


def greaterOrEqual(v, l, r, x, lx, rx):
    if l >= rx or lx >= r:
        return -1
    if l <= lx and rx <= r:
        if a[x] < v:
            return -1
        while rx - lx != 1:
            mid = (rx + lx + 1) // 2
            if a[2 * x + 1] >= v:
                x = 2 * x + 1
                rx = mid
            else:
                x = 2 * x + 2
                lx = mid
        return lx
    mid = (lx + rx + 1) // 2
    left = greaterOrEqual(v, l, r, 2 * x + 1, lx, mid)
    if left != -1:
        return left
    return greaterOrEqual(v, l, r, 2 * x + 2, mid, rx)


def dump():
    for i in range(size):
        print(a[i + size - 1], end=' ')
    print()


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
s = [int(i) for i in input().split()]
a = [-math.inf for _ in range(size * 2 - 1)]
for i in range(n):
    update(i, s[i], 0, size, 0)

for i in range(m):
    req = [int(j) for j in input().split()]
    if req[0] == 1:
        update(req[1], req[2], 0, size, 0)
    else:
        print(greaterOrEqual(req[1], req[2], size, 0, 0, size))