f = open("input.txt")
n, k = [int(i) for i in f.readline().split()]
a = [0] + [int(i) for i in f.readline().split()] + [0]
dp = [0 for i in range(len(a))]
p = [-1 for i in range(len(a) - 1)]
p[0] = 0
jumps = []
for i in range(1, n):
    dp[i] = -10001
    for j in range(1, k + 1):
        if i - j >= 0:
            if dp[i - j] + a[i] > dp[i]:
                dp[i] = dp[i - j] + a[i]
                p[i - 1] = i - j + 1


p += [n]
with open("output.txt", "w") as f:
    f.write(str(dp[-1]) + '\n')
    f.write(str(len(set(p)) - 1) + '\n')
    f.write(' '.join([str(i) for i in set(p)]))