n, s = [int(i) for i in input().split()]
w = [int(i) for i in input().split()]
a = [0 for i in range(s + 1)]
a[0] = 1
dop = [[] for i in range(s + 1)]
for i in range(len(w)):
    for j in reversed(range(w[i], len(a))):
        if a[j - w[i]] == 1 and j - w[i] >= 0:
            a[j] = 1
            dop[j] = dop[j - w[i]] + [w[i]]

m = 0
for i in range(s + 1):
    if a[i] == 1:
        m = max(m, i)

print(m)
print(len(dop[m]))
print(*dop[m])