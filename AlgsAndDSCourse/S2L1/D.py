import math


def set(i, v, l, r, x):
    if r - l == 1:
        a[x] = v
    else:
        mid = (r + l + 1) // 2
        if i < mid:
            set(i, v, l, mid, 2 * x + 1)
        else:
            set(i, v, mid, r, 2 * x + 2)
        a[x] = a[2 * x + 1] + a[2 * x + 2]


def kOne(x, c, lx, rx):
    left = 2 * x + 1
    right = 2 * x + 2
    mid = (lx + rx + 1) // 2
    if mid - lx == 1:
        if (rx - lx == 1):
            return lx
        if a[left] == c:
            return lx
        elif a[left] < c:
            return rx - 1
        else:
            print("shit...")
            return None
    # print(lx, mid, rx)
    if c <= a[left]:
        return kOne(left, c, lx, mid)
    else:
        return kOne(right, c - a[left], mid, rx)


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
s = [int(i) for i in input().split()]
a = [0 for _ in range(size * 2 - 1)]
for i in range(n):
    set(i, s[i], 0, size, 0)

for i in range(m):
    req = [int(j) for j in input().split()]
    if req[0] == 1:
        set(req[1], int(a[req[1] + size - 1] != 1), 0, size, 0)
    else:
        print(kOne(0, req[1] + 1, 0, size))