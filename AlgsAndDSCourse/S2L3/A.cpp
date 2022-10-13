import math


n = int(input())
depth = int(math.log2(n + 1)) + 1
p = [int(i) for i in input().split()]
p = [0] + p
dp = [[0 for _ in range(depth)] for _ in range(n + 1)]

for v in range(1, n + 1):
    dp[v][0] = p[v]

for i in range(1, depth):
    for v in range(1, n + 1):
        dp[v][i] = dp[dp[v][i - 1]][i - 1]

for v in range(1, n + 1):
    print(f'{v}:', end='')
    for i in dp[v]:
        print(f' {i}' if i != 0 else '', end='')
    print()
