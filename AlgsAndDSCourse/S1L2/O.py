n, s = [int(i) for i in input().split()]
w = [int(i) for i in input().split()]
a = [0 for i in range(s + 1)]
a[0] = 1
dop = [[0 for _ in range(101)] for i in range(s + 1)]
dop[0] = []
for i in range(len(w)):
    for j in reversed(range(w[i], len(a))):
        if a[j - w[i]] == 1 and j - w[i] >= 0:
            a[j] = 1
            if len(dop[j - w[i]]) + 1 < len(dop[j]):
                dop[j] = dop[j - w[i]] + [w[i]]

if a[s] == 1:
    print(len(dop[s]))
else:
    print(0)