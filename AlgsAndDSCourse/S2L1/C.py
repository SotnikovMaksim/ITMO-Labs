import math


def update(i, v, l, r, x):
    if r - l == 1:
        a[x] = [v, max(0, v), max(0, v), max(0, v)]
        return
    mid = (r + l + 1) // 2
    if i < mid:
        update(i, v, l, mid, 2 * x + 1)
    else:
        update(i, v, mid, r, 2 * x + 2)
    a[x] = compare(a[2 * x + 1], a[2 * x + 2])


def dump():
    print("array:")
    for i in range(size):
        print(a[i + size - 1][0], end=' ')
    print()


def compare(left, right):  # [sum on whole segment, prefix sum, postfix sum, best sum on subsegment
    res = []
    res.append(left[0] + right[0])  # common sum of both segments
    res.append(max(left[1], left[0] + right[1]))  # pref: left prefix sum, sum of whole left segment + right prefix sum
    res.append(max(right[2], left[2] + right[0]))  # post: right postfix sum, left postfix sum + whole right segment sum
    res.append(max(max(left[3], right[3]), left[2] + right[1]))  # subsegment sum: left sub sum, right sub sum, left post + right pref
    return res


n, m = [int(i) for i in input().split()]
size = 2 ** (math.ceil(math.log2(n)))
s = [int(i) for i in input().split()]
a = [[0, 0, 0, 0] for _ in range(size * 2 - 1)]
for i in range(n):
    update(i, s[i], 0, size, 0)

# dump()
print(a[0][3])
for i in range(m):
    req = [int(j) for j in input().split()]
    update(req[0], req[1], 0, size, 0)
    # dump()
    print(a[0][3])