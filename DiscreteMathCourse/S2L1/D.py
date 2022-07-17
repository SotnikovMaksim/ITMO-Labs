def getDelta(m, f):
    delta = dict()
    for i in range(m):
        fr, to, s = f.readline().split()
        if int(fr) in delta:
            delta[int(fr)][s] = int(to)
        else:
            delta[int(fr)] = dict()
            delta[int(fr)][s] = int(to)
    return delta
 
 
def dump(a):
    for i in a:
        print(*i)
 
 
def solve():
    f = open("problem4.in", "r")
    n, m, k, l = [int(i) for i in f.readline().split()]
    terminal = [int(i) for i in f.readline().split()]
    if len(terminal) == 0:
        return 0
    MOD = 10 ** 9 + 7
    delta = getDelta(m, f)
    f.close()
    dp = [[[False, 0] for i in range(n)] for j in range(l + 1)]
    dp[0][0] = [True, 1]
    for i in range(l):
        for j in range(n):
            if dp[i][j][0]:
                if j + 1 not in delta.keys():
                    continue
                for s in delta[j + 1].keys():
                    dp[i + 1][delta[j + 1][s] - 1][0] = True
                    dp[i + 1][delta[j + 1][s] - 1][1] += dp[i][j][1]
 
    ans = 0
    for term in terminal:
        ans += dp[l][term - 1][1]
 
    return ans % MOD
 
 
ans = solve()
with open("problem4.out", "w") as f:
    f.write(str(ans))
    f.close()