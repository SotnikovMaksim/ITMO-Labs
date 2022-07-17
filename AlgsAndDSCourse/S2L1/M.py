n, m = [int(i) for i in input().split()]
borders = []
c = 0

for i in range(n):
    leftBorder, rightBorder = [int(i) for i in input().split()]
    if leftBorder > rightBorder:
        leftBorder, rightBorder = rightBorder, leftBorder
    borders.append([leftBorder, -1])
    borders.append([rightBorder, 1])

dots = [int(i) for i in input().split()]
for i in range(m):
    borders.append([dots[i], 0, i])

#  c - текущее количсетво прямых, в которых мы находимся. находим точку - добавляем в ответ на её запрос с
ans = [0 for _ in range(m)]

borders.sort()
c = 0
for border in borders:
    c += -(border[1])
    if border[1] == 0:
        ans[border[2]] = c

print(*ans)