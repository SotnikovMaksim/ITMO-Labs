n = int(input())
a = [int(i) for i in input().split()]

d = [1 for i in range(n)]
seq = [[i] for i in a]
for i in range(n):
    for j in range(i):
        if d[j] + 1 > d[i] and a[i] > a[j]:
            seq[i] = seq[j] + [a[i]]
            d[i] = d[j] + 1

ans = []
m = 0
for i in range(n):
    if d[i] > m:
        m = d[i]
        ans = seq[i]

print(len(ans))
print(*ans)