f = open("input.txt")
n = int(f.read())
c = 2
a = [0 for i in range(n + 1)]
a[0], a[1] = 1, 2
for i in range(2, n + 1):
    a[i] = a[i - 1] + a[i - 2]

with open("output.txt", mode="w") as f:
    f.write(str(a[-1]))