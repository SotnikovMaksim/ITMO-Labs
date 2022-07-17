def mod_merge(a):
    if len(a) <= 1:
        return a, 0
    left, inversions_left = mod_merge(a[:(len(a)//2)])
    right, inversions_right = mod_merge(a[(len(a)//2):])
    array, inversions = merge(left,right)
    return [array, (inversions_left + inversions_right + inversions)]
 
 
def merge(a, b):
    array = []
    counter = 0
    i = 0
    j = 0
    while i < len(a) and j < len(b):
        if a[i] <= b[j]:
            array.append(a[i])
            i += 1
        else:
            array.append(b[j])
            j += 1
            counter += len(a) - i
    if i == len(a):
        array += b[j:]
    else:
        array += a[i:]
    return array, counter
 
n = int(input())
a = [int(i) for i in input().split()]
 
print(mod_merge(a)[1])