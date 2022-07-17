f = open("knight.in")
n, m = [int(i) for i in f.read().split()]
a = [[0 for j in range(m + 1)] for i in range(n + 2)]
a[0], a[1] = [-1 for i in range(1, m + 1)], [-1 for i in range(1, m + 1)]
for i in range(n + 2):
    a[i][0] = -1
a[2][1] = 1
for row in range(2, n + 2):
    for col in range(1, m + 1):
        if a[row - 2][col - 1] > 0:
            a[row][col] += a[row - 2][col - 1]
        if a[row - 1][col - 2] > 0:
            a[row][col] += a[row - 1][col - 2]

with open("knight.out", mode="w") as f:
    f.write(str(a[-1][-1]))