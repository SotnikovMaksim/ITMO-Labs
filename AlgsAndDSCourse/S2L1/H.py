import math

n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
a = [[math.inf, 0] for _ in range(size * 2 - 1)]


# [min, max, count]
def update(i, v, lx, rx, x):
    if rx - lx == 1:
        a[x][0] = v
        a[x][1] = 1
        return
    mid = (rx + lx + 1) // 2
    if i < mid:
        update(i, v, lx, mid, 2 * x + 1)
    else:
        update(i, v, mid, rx, 2 * x + 2)
    a[x][0] = min(a[2 * x + 1][0], a[2 * x + 2][0])
    a[x][1] = a[2 * x + 1][1] + a[2 * x + 2][1]


def earthquake(l, r, v, lx, rx, x):
    if r <= lx or rx <= l:
        return 0
    if rx - lx == 1 and l <= lx and rx <= r:
        if a[x][0] - v <= 0:
            a[x][0] = math.inf
            a[x][1] = 0
            return 1
        else:
            return 0
    mid = (rx + lx + 1) // 2
    ans = 0
    if v >= a[2 * x + 1][0]:
        ans += earthquake(l, r, v, lx, mid, 2 * x + 1)
    if v >= a[2 * x + 2][0]:
        ans += earthquake(l, r, v, mid, rx, 2 * x + 2)
    a[x][0] = min(a[2 * x + 1][0], a[2 * x + 2][0])
    a[x][1] = a[2 * x + 1][1] + a[2 * x + 2][1]
    return ans


# DEBUG FUNCTION
def dump():
    print("array:")
    for i in range(size):
        print(a[i + size - 1][0], end=' ')
    print()


for i in range(m):
    req = [int(j) for j in input().split()]
    if req[0] == 1:
        update(req[1], req[2], 0, size, 0)
    else:
        print(earthquake(req[1], req[2], req[3], 0, size, 0))