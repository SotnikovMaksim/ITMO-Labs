n = int(input())
ans = []
while n % 2 == 0:
    n //= 2
    ans.append(2)

for p in range(3, int(n ** 0.5) + 1, 2):
    while n % p == 0:
        ans.append(p)
        n //= p
if n > 2:
    ans.append(n)

print(*ans)