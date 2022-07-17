def ero(a):
    n = max(a)
    mp = [-1 for _ in range(n + 1)]
    dp = [0 for _ in range(n + 1)]
    for i in a:
        dp[i] = i
    dict = {}
    for i in a:
        dict[i] = []
    for i in range(2, n + 1):
        if mp[i] == -1:
            k = 1
            while k * i <= n:
                if dp[k * i] > 0:
                    while dp[k * i] % i == 0:
                        dp[k * i] //= i
                        dict[k * i].append(i)
                k += 1
    ans = []
    for i in a:
        ans.append(dict[i])
    return ans

n = int(input())
a = [int(input()) for i in range(n)]

primes = ero(a)
for i in range(n):
    print(*primes[i])