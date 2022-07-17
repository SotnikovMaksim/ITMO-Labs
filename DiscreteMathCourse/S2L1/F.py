def getDelta(f, n):
    delta = dict()
 
    for i in range(n):
        req = f.readline().split()
        fr = int(req[0])
        to = int(req[1])
        s = req[-1]
        if fr not in delta.keys():
            delta[fr] = dict()
        delta[fr][s] = to
 
    return delta
 
 
def data(f):
    n, m, k = [int(i) for i in f.readline().split()]
    t = [int(i) for i in f.readline().split()]
    delta = getDelta(f, m)
    return n, m, k, t, delta
 
 
def dfs(u, v):
    if visited[u] and visited[v]:
        return True
    visited[u] = True
    visited[v] = True
    if (u in t1) != (v in t2):
        return False
    if (u in delta1.keys()) != (v in delta2.keys()):
        return False
    if u not in delta1.keys():
        return True
    ans = True
    for c in delta1[u].keys():
        if c not in delta2[v]:
            return False
        next1 = delta1[u][c]
        next2 = delta2[v][c]
        if not visited[next1]:
            ans = dfs(next1, next2) and ans
    return ans
 
 
f = open("isomorphism.in", "r")
 
n1, m1, k1, t1, delta1 = data(f)
n2, m2, k2, t2, delta2 = data(f)
 
f.close()
visited = [False for i in range(max(n1, n2) + 1)]
ans = dfs(1, 1)
 
with open("isomorphism.out", "w") as f:
    if ans:
        f.write("YES")
    else:
        f.write("NO")