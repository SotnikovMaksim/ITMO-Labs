f = open("input.txt")
n, m = [int(i) for i in f.readline().split()]
a = [[int(j) for j in f.readline().split()] for i in range(n)]
ans = [[0 for col in range(m)] for row in range(n)]
for r in range(n):
    for c in range(m):
        if (r > 0) and (c > 0):
            ans[r][c] = max(ans[r - 1][c], ans[r][c - 1]) + a[r][c]
        elif r > 0:
            ans[r][c] = ans[r - 1][c] + a[r][c]
        else:
            ans[r][c] = ans[r][c - 1] + a[r][c]

r, c = n - 1, m - 1
turns = []
while (r != 0 and c != 0):
    if ans[r - 1][c] >= ans[r][c - 1]:
        r -= 1
        turns.append('D')
    else:
        c -= 1
        turns.append('R')

while r > 0:
    turns.append('D')
    r -= 1
while c > 0:
    turns.append('R')
    c -= 1

with open("output.txt", "w") as f:
    f.write(str(ans[-1][-1]))
    f.write("\n")
    for i in reversed(range(len(turns))):
        f.write(turns[i])