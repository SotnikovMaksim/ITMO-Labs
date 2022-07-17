def dop(i, j):
    global dp
    if j > i:
        return 10 ** 6
    cost = a[i]
    if j <= 0:
        if i >= 1:
            if cost <= 100:
                res = min(dop(i - 1, j + 1), dop(i - 1, j) + cost)
            else:
                return dop(i - 1, j + 1)
        else:
            return 0
    else:
        if dp[i][j] != -1:
            return dp[i][j]
        if cost > 100:
            res = min(dop(i - 1, j + 1), dop(i - 1, j - 1) + cost)
        else:
            res = min(dop(i - 1, j + 1), dop(i - 1, j) + cost)
    dp[i][j] = res
    return res


def prevDays(i, j):
    global a, was
    if j < i:
        c = a[i]
        if j <= 0:
            if i >= 1:
                if c > 100:
                    was.append(i)
                    prevDays(i - 1, j + 1)
                else:
                    temp = dop(i, j) == dop(i - 1, j + 1)
                    if temp:
                        was.append(i)
                        prevDays(i - 1, j + 1)
                    else:
                        prevDays(i - 1, j)
        else:
            if c <= 100:
                temp = dop(i - 1, j + 1) == dop(i, j)
                if temp:
                    was.append(i)
                    prevDays(i - 1, j + 1)
                else:
                    prevDays(i - 1, j)
            else:
                temp = dop(i - 1, j + 1) == dop(i, j)
                if temp:
                    was.append(i)
                    prevDays(i - 1, j + 1)
                else:
                    prevDays(i - 1, j - 1)


n = int(input())
k1 = 0
k2 = 0
a = [int(input()) for i in range(n)]
a = [0] + a
dp = [[-1 for _ in range(n + 2)] for j in range(n + 1)]
ans = 10 ** 6

for i in range(n + 1):
    temp = dop(n, i)
    if temp <= ans:
        ans = temp
        k1 = i

print(ans)

was = []
prevDays(n, k1)

k2 = len(was)
print(k1, k2)

for i in reversed(range(len(was))):
    print(was[i])