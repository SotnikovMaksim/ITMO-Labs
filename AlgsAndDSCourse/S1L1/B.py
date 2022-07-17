n = int(input())
a = [int(i) for i in input().split()]
c = [0 for i in range(101)]
 
for i in range(len(a)):
    c[a[i]] += 1
 
for i in range(len(c)):
    if c[i] != 0:
        for j in range(c[i]):
            print(i, end=" ")