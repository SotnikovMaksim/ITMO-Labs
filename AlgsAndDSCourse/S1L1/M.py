def bs(a, n):
    l = 0
    r = len(a) - 1
    diff = 10 ** 10
    ans = 10 ** 10
    while r > l:
        mid = (r + l) // 2
        if abs(a[mid] - n) <= diff:
            if abs(a[mid] - n) < abs(ans - n):
                ans = a[mid]
            elif abs(a[mid] - n) == abs(ans - n):
                ans = min(ans, a[mid])
            diff = abs(a[mid] - n)
        if n < a[mid]:
            r = mid
        elif n > a[mid]:
            l = mid + 1
        else:
            return ans
    if abs(a[l] - n) <= diff:
        if abs(a[l] - n) < abs(ans - n):
            ans = a[l]
        elif abs(a[l] - n) == abs(ans - n):
            ans = min(ans, a[l])
    return ans

n, k = [int(i) for i in input().split()]
a = [int(i) for i in input().split()]
f = [int(i) for i in input().split()]
for i in range(k):
    print(bs(a, f[i]))