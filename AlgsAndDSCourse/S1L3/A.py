def find(x):
    global p
    if p[x] != x:
        p[x] = find(p[x])
    return p[x]


def union(x, y):
    p[x] = y


n = int(input())
a = [int(i) for i in input().split()]
p = list(range(n + 1))
free = list(range(1, n + 1))

for i in range(n):
    ans = find(a[i])
    print(ans, end=' ')
    if ans < n:
        union(ans, ans + 1)
    else:
        union(ans, 1)