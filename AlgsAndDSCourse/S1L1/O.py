import math


def equation(x):
    return float(x ** 2 + math.sqrt(x))


def bs(c):
    l = 1
    r = 10 ** 38
    e = 10 ** 19
    while r - l > 1:
        middle = float((r + l) / (2 * e))
        if equation(middle) == c:
            return middle
        elif equation(middle) > c:
            r = (l + r) // 2
        elif equation(middle) < c:
            l = ((l + r) // 2) + 1
    return float(l / e)


c = float(input())
print(bs(c))