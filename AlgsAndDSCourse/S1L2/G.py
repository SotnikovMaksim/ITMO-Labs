f = open("knight.in")
n, m = [int(i) for i in f.read().split()]

a = [[0 for i in range(m + 4)] for j in range(n + 4)]
a[0], a[1], a[-1], a[-2] = [-1 for i in range(m + 4)], [-1 for i in range(m + 4)], [-1 for i in range(m + 4)], [-1 for i in range(m + 4)]
for i in range(len(a)):
    a[i][0], a[i][1], a[i][-1], a[i][-2] = -1, -1, -1, -1

a[2][2] = 1

for k in range(2, m + 2):
    i = 2
    j = k
    while j > 1 and i < n + 2:
        if a[i - 2][j + 1] > 0:
            a[i][j] += a[i - 2][j + 1] % (10 ** 6 + 7)
        if a[i - 2][j - 1] > 0:
            a[i][j] += a[i - 2][j - 1] % (10 ** 6 + 7)
        if a[i - 1][j - 2] > 0:
            a[i][j] += a[i - 1][j - 2] % (10 ** 6 + 7)
        if a[i + 1][j - 2] > 0:
            a[i][j] += a[i + 1][j - 2] % (10 ** 6 + 7)
        i += 1
        j -= 1

for k in range(3, n + 2):
    j = m + 1
    i = k
    while j > 1 and i < n + 2:
        if a[i - 2][j + 1] > 0:
            a[i][j] += a[i - 2][j + 1] % (10 ** 6 + 7)
        if a[i - 2][j - 1] > 0:
            a[i][j] += a[i - 2][j - 1] % (10 ** 6 + 7)
        if a[i - 1][j - 2] > 0:
            a[i][j] += a[i - 1][j - 2] % (10 ** 6 + 7)
        if a[i + 1][j - 2] > 0:
            a[i][j] += a[i + 1][j - 2] % (10 ** 6 + 7)
        i += 1
        j -= 1


with open("knight.out", mode="w") as f:
    f.write(str(a[n + 1][m + 1] % (10 ** 6 + 7)))