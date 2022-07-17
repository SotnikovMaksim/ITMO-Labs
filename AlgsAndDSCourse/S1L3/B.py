def find(x):
    global p
    if p[x] != x:
        p[x] = find(p[x])
    return p[x]


def union(x, y):
    if find(x) == find(y):
        return
    global p, rank, uns
    x = find(x)
    y = find(y)
    if rank[x] > rank[y]:
        x, y = y, x
    p[x] = y
    uns[y] += uns[x]
    uns[x] = []
    if rank[x] == rank[y]:
        rank[y] += 1


def add(x, v):
    global exp, uns
    u = uns[find(x)]
    for i in range(len(u)):
        exp[u[i]] += v


n, r = [int(i) for i in input().split()]
p = list(range(n + 1))
uns = [[i] for i in range(n + 1)]
exp = [0 for _ in range(n + 1)]
rank = [0] + [1 for _ in range(1, n + 1)]

for i in range(r):
    line = input().split()
    if line[0] == 'add':
        add(int(line[1]), int(line[2]))
    elif line[0] == 'get':
        print(exp[int(line[1])])
    elif line[0] == 'join':
        union(int(line[1]), int(line[2]))