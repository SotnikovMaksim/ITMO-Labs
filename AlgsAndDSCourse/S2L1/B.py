import math


def set(i, v, l, r, x):
    if r - l == 1:
        a[x][0] = v
        a[x][1] = 1
    else:
        mid = (r + l + 1) // 2
        if i < mid:
            set(i, v, l, mid, 2 * x + 1)
        else:
            set(i, v, mid, r, 2 * x + 2)
        a[x][0] = min(a[2 * x + 1][0], a[2 * x + 2][0])
        if a[2 * x + 1][0] == a[2 * x + 2][0]:
            a[x][1] = a[2 * x + 1][1] + a[2 * x + 2][1]
        else:
            a[x][1] = a[2 * x + 1][1] * (a[x][0] == a[2 * x + 1][0]) + a[2 * x + 2][1] * (a[x][0] == a[2 * x + 2][0])


def minim(l, r, x, lx, rx):
    if l >= rx or lx >= r:
        return [math.inf, 0]
    if lx >= l and rx <= r:
        return [a[x][0], a[x][1]]
    if rx - lx <= 1:
        return [a[lx + size - 1][0], a[lx + size - 1][1]]
    mid = (lx + rx + 1) // 2
    left = minim(l, r, 2 * x + 1, lx, mid)
    right = minim(l, r, 2 * x + 2, mid, rx)
    if left[0] == right[0]:
        return [left[0], left[1] + right[1]]
    elif left[0] < right[0]:
        return left
    else:
        return right


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
s = [int(i) for i in input().split()]
a = [[math.inf, 0] for _ in range(size * 2 - 1)]
for i in range(n):
    set(i, s[i], 0, size, 0)

for i in range(m):
    req = [int(j) for j in input().split()]
    if req[0] == 1:
        set(req[1], req[2], 0, size, 0)
    else:
        print(*minim(req[1], req[2], 0, 0, size))