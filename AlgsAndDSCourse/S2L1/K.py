import math


def minim(t, u, v, logs):
    if u > v:
        u = u + v
        v = u - v
        u = u - v
    p = logs[v - u]
    return min(t[p][u], t[p][v - (1 << p) + 1])


def build(n):
    deep = int(math.log(n, 2)) + 1
    t = [[-1 for j in range(n + 1)] for i in range(deep)]
    t[0][0] = f
    for i in range(1, n):
        t[0][i] = ((23 * t[0][i - 1] + 21563) % 16714589)
    for i in range(1, deep):
        for j in range(n):
            if j + (1 << i - 1) <= n:
                t[i][j] = min(t[i - 1][j], t[i - 1][j + (1 << (i - 1))])
    return t


n, m, f = [int(i) for i in input().split()]
u, v = [int(i) for i in input().split()]
t = build(n)
logs = [int(math.log2(i)) for i in range(1, n + 1)]
r = minim(t, u - 1, v - 1, logs)

# Requests generation
for i in range(1, m):
    u = ((17 * u + 751 + r + 2 * i) % n) + 1
    v = ((13 * v + 593 + r + 5 * i) % n) + 1
    r = minim(t, u - 1, v - 1, logs)
print(u, v, r)