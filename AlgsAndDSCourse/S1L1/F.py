def sift_down(a):
    if len(a) <= 1:
        return a
    cur = 0
    while True:
        left = cur * 2 + 1
        right = cur * 2 + 2
        maximum = cur
        if (left < len(a)) and (a[left] > a[maximum]):
            maximum = left
        if (right < len(a)) and (a[right] > a[maximum]):
            maximum = right
        if maximum == cur:
            break
        a[cur], a[maximum] = a[maximum], a[cur]
        cur = maximum
    return a

def sift_up(a, i):
    if len(a) <= 1:
        return a
    cur = len(a) - 1
    while True:
        parent = (cur - 1) // 2
        choice = cur
        if (parent >= 0) and (a[parent] < a[cur]):
            choice = parent
        if choice == cur:
            break
        a[cur], a[parent] = a[parent], a[cur]
        cur = parent
    return a

def insert(a, n):
    a.append(n)
    a = sift_up(a, 0)
    return a


def extract(a):
    if len(a) == 1:
        return a.pop(0)
    ans = a[0]
    a[0] = a[-1]
    a.pop(-1)
    sift_down(a)
    return ans


n = int(input())
a = []
for i in range(n):
    command = [int(i) for i in input().split()]
    if command[0] == 0:
        a = insert(a, command[1])
    else:
        print(extract(a))