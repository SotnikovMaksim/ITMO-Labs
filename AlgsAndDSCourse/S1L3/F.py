n = int(input())
a = [int(input()) for i in range(n)]

def ero(n):
    primes = [2]
    mp = [-1 for _ in range(n + 1)]
    for i in range(3, n + 1, 2):
        if mp[i] == -1:
            primes.append(i)
            mp[i] = i
        for p in primes:
            if p > mp[i] or i * p > n:
                break
            mp[i * p] = p
    return mp


primes = ero(max(a))
for i in range(n):
    if primes[a[i]] == a[i]:
        print('YES')
    else:
        print('NO')




# return ~-2 ** n % n < 2