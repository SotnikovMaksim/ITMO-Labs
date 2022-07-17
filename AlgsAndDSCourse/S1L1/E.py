n = int(input())
a = [int(i) for i in input().split()]
flag = True
for i in range(n):
    if i * 2 + 1 < n:
        if a[i] > a[i * 2 + 1]:
            flag = False
    if i * 2 + 2 < n:
        if a[i] > a[i * 2 + 2]:
            flag = False
if flag:
    print('YES')
else:
    print('NO')
