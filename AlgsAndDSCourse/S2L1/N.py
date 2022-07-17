import math


# сортировка здесь очевидно не нужна
def qsort(a):
    if len(a) <= 1:
        return a
    x = a.pop(len(a) // 2)
    g = []  # greater
    le = []  # less or equals
    for i in a:
        if (i[0] < x[0]) or (i[0] == x[0] and i[1] > x[1]):
            le.append(i)
        else:
            g.append(i)
    return qsort(le) + [x] + qsort(g)


def divide(left, right):
    global c
    left = 3600 * left[0] + 60 * left[1] + left[2]
    right = 3600 * right[0] + 60 * right[1] + right[2]
    timeLine.append([left, 1])
    timeLine.append([right, -1])
    if left >= right:
        c += 1


n = int(input())
timeLine = []
ans = 0
c = 0

for i in range(n):
    boxRange = [int(i) for i in input().split()]
    leftRange = boxRange[:3]
    rightRange = boxRange[3:]
    divide(leftRange, rightRange)


timeLine = sorted(timeLine)
s = 0
for time in timeLine:
    c += time[1]
    if c == n:
        s = time[0]
    if c == n - 1 and time[1] == -1:
        ans += time[0] - s

print(ans + (24 * 60 * 60 - s) * int(c == n))