n = int(input())
a = [int(i) for i in input().split()]
b = []
j = 0
need = 1
flag = True
ans = []
while j < n:
    if len(a) > 0:
        b.append(a.pop(0))
        j += 1
        ans.append((1, 1))
    while len(b) > 0 and b[-1] == need:
        b.pop(-1)
        need += 1
        ans.append((2, 1))
    if j == n and len(b) > 0 and b[-1] != need:
        flag = False
        break


if flag:
    for i in ans:
        print(*i)
else:
    print(0)