k, s = [int(i) for i in input().split()]
dp = [[0 for j in range(s + 1)] for i in range(k)]
for i in range(min(s + 1, 10)):
    dp[0][i] = 1

for i in range(1, k):
    for j in range(s + 1):
        for c in range(10):
            if j - c >= 0:
                dp[i][j] += (dp[i - 1][j - c]) % (10 ** 9 + 7)

print((dp[-1][-1]) % (10 ** 9 + 7))