n = int(input())
a = [int(i) for i in input().split()]
 
 
def msort(a):
    if len(a) <= 1:
        return a
    left = msort(a[:len(a) // 2])
    right = msort(a[len(a) // 2:])
    ans = []
    i, j = 0, 0
    while i < len(left) and j < len(right):
        if left[i] < right[j]:
            ans.append(left[i])
            i += 1
        else:
            ans.append(right[j])
            j += 1
 
    while i < len(left):
        ans.append(left[i])
        i += 1
 
    while j < len(right):
        ans.append(right[j])
        j += 1
 
    return ans
 
print(*msort(a))

