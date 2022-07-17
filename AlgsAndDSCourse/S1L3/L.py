def gcd(a, b):
    if a < b:
        a, b = b, a
    if a ==0 or b == 0:
        return a
    return gcd(b, a % b)


d, k = [int(i) for i in input().split()]


if d == k:
    print(d, d)
else:
    m = d * k
    pairs = []
    rev_pairs = []
    i = d
    dec_s = m ** 0.5
    s = int(dec_s)
    if dec_s % 1 == 0:
        while i < s:
            if m % i == 0:
                mi = m // i
                if gcd(i, mi) == d:
                    pairs.append([i, m // i])
                    rev_pairs.append([m // i, i])
            i += d
        if s == d:
            pairs.append([s, s])
    else:
        while i <= s:
            if m % i == 0:
                mi = m // i
                if gcd(i, mi) == d:
                    pairs.append([i, m // i])
                    rev_pairs.append([m // i, i])
            i += d
    for i in range(len(pairs)):
        print(*pairs[i])
    for i in reversed(range(len(rev_pairs))):
        print(*rev_pairs[i])