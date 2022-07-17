n, m = [int(i) for i in input().split()]
a = [int(i) for i in input().split()]
s = [sum(a)]
for i in range(len(a) - 1):
    s.append(s[i] - a[i])
s.append(0)
for i in range(m):
    l, r = [int(i) for i in input().split()]
    print(s[l - 1] - s[r])