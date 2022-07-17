f = open("input.txt")
n = int(f.read())
a = [0 for i in range(n)]
a[0] = 1
for i in range(1, n):
    a[i] = a[i - 1] + a[i - 2]

with open("output.txt", mode="w") as f:
    f.write(str(a[-1]))