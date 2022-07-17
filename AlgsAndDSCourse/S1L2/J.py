n, m = [int(i) for i in input().split()]
a = [[int(j) for j in input().split()] for i in range(n)]
dp = [[0 for i in range(m)] for j in range(n)]

dp[0] = a[0]
for i in range(n):
    dp[i][0] = a[i][0]


for i in range(1, len(a)):
    for j in range(1, len(a[i])):
        if a[i][j] != 0:
            dp[i][j] = min(dp[i - 1][j - 1], dp[i][j - 1], dp[i - 1][j]) + 1

m = -1
for i in range(len(dp)):
    m = max(max(dp[i]), m)

print(m)