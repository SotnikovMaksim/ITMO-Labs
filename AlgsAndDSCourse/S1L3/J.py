from collections import Counter

n = int(input())
a = [int(i) for i in input().split()]
def gcd(a, b):
    if a < b:
        a, b = b, a
    if a == 0 or b == 0:
        return a
    else:
        return gcd(b, a % b)

targ = a[0]
ans = 0
if n == 1:
    print(a[0])
else:
    for i in range(1, n):
        ans = gcd(targ, a[i])
        targ = ans
    print(ans)