def getDelta(f, n):
    delta = dict()
 
    alpha = set()
    for i in range(n):
        fr, to, s = f.readline().split()
        if int(fr) in delta:
            delta[int(fr)][s] = int(to)
        else:
            delta[int(fr)] = dict()
            delta[int(fr)][s] = int(to)
        alpha.add(s)
    return delta, alpha
 
 
def data(f):
    n, m, k = [int(i) for i in f.readline().split()]
    t = [int(i) for i in f.readline().split()]
    delta, alpha = getDelta(f, m)
    return n, m, k, t, delta, alpha
 
 
def diavel(v, delta):
    if v not in delta.keys():
        return True
    for c in delta[v].keys():
        if delta[v][c] != v:
            return False
    return True
 
 
def dfs(u, v):
    if visited[u][v]:
        return True
    visited[u][v] = True
    if (u in t1) != (v in t2):
        return False
    ans = True
    for c in alpha:
        if u not in delta1 or c not in delta1[u]:
            next1 = 0
        else:
            next1 = delta1[u][c]
        if v not in delta2 or c not in delta2[v]:
            next2 = 0
        else:
            next2 = delta2[v][c]
        ans = dfs(next1, next2) and ans
    return ans
 
 
f = open("equivalence.in", "r")
 
n1, m1, k1, t1, delta1, alpha1 = data(f)
n2, m2, k2, t2, delta2, alpha2 = data(f)
alpha = alpha1.union(alpha2)
 
visited = [[False for j in range(n2 + 1)] for i in range(n1 + 1)]
ans = dfs(1, 1)
 
f.close()
with open("equivalence.out", "w") as f:
    if ans:
        f.write("YES")
    else:
        f.write("NO")
    f.close()