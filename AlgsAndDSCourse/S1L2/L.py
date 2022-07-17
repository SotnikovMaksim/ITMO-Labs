def leftBs(a, n):
    t = n + 1
    l = -1
    r = len(a)
    while True:
        while r - l > 1:
            mid = (l + r) // 2
            if t <= a[mid]:
                r = mid
            else:
                l = mid
        if r == len(a):
            l = -1
            r = len(a)
            t += 1
        else:
            return r


n, a, k, b, m = [int(i) for i in input().split()]
seq = [0 for i in range(n)] + [10 ** 5]
seq[0] = a
for i in range(1, n):
    seq[i] = (k * seq[i - 1] + b) % m

dp = [0] * (max(seq) + 100)
dp[0] = -10 ** 15
for i in range(1, len(dp)):
    dp[i] = 10 ** 15

for i in range(n):
    s = leftBs(dp, seq[i])
    if dp[s - 1] < seq[i] and seq[i] <= dp[s]:
        dp[s] = seq[i]

counter = 0
for i in range(len(dp)):
    if dp[i] != -10 ** 15 and dp[i] != 10 ** 15:
        counter += 1

print(counter)