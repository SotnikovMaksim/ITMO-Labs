a = list(input())
b = list(input())

d = [[0 for c in range(len(a) + 1)] for r in range(len(b) + 1)]

for i in range(len(a) + 1):
    d[0][i] = i
for i in range(len(b) + 1):
    d[i][0] = i

for i in range(1, len(b) + 1):
    for j in range(1, len(a) + 1):
        if a[j - 1] == b[i - 1]:
            d[i][j] = d[i - 1][j - 1]
        else:
            d[i][j] = 1 + min(d[i - 1][j - 1], d[i - 1][j], d[i][j - 1])

print(d[-1][-1])