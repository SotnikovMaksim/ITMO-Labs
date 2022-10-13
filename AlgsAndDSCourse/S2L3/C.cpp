import math


def cheapest(u, v):
    if d[v] > d[u]:
        u, v = v, u
    diff = d[u] - d[v]
    ans = math.inf
    if diff != 0:
        for i in reversed(range(depth)):
            if d[dp[u][i][0]] >= d[v]:
                ans = min(ans, dp[u][i][1])
                u = dp[u][i][0]

    if u == v:
        return ans

    for i in reversed(range(depth)):
        if dp[u][i] != dp[v][i]:
            ans = min(ans, dp[u][i][1])
            ans = min(ans, dp[v][i][1])
            u = dp[u][i][0]
            v = dp[v][i][0]

    return ans


n = int(input())
depth = int(math.log2(n + 1)) + 1
p = [[int(i) for i in input().split()] for _ in range(n - 1)]
p = [[1, math.inf], [1, math.inf]] + p
d = [0 for _ in range(n + 1)]
m = int(input())
dp = [[(0, 0) for _ in range(depth)] for _ in range(n + 1)]

for i in range(2, n + 1):
    d[i] = d[p[i][0]] + 1

for v in range(1, n + 1):
    dp[v][0] = (p[v][0], p[v][1])

for i in range(1, depth):
    for v in range(1, n + 1):
        state = dp[dp[v][i - 1][0]][i - 1][0]
        edge = min(dp[dp[v][i - 1][0]][i - 1][1], dp[v][i - 1][1])
        dp[v][i] = (state, edge)

for i in range(m):
    req = [int(i) for i in input().split()]
    print(cheapest(req[0], req[1]))
