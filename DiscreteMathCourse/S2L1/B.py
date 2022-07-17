def getDelta(m, f):
    delta = dict()
    for i in range(m):
        fr, to, s = f.readline().split()
        if int(fr) not in delta.keys():
            delta[int(fr)] = dict()
            delta[int(fr)][s] = set()
        if s not in delta[int(fr)].keys():
            delta[int(fr)][s] = set()
        delta[int(fr)][s].add(int(to))
 
    return delta
 
 
def solve():
    f = open("problem2.in", "r")
    word = f.readline().strip()
    n, m, k = [int(i) for i in f.readline().split()]
    terminals = [int(i) for i in f.readline().split()]
    s = 0
 
    if len(terminals) == 0:
        print("Rejects")
    delta = getDelta(m, f)
    f.close()
 
    possible = [[False for _ in range(n)] for _ in range(len(word) + 1)]
    possible[0][s] = True
    for i in range(len(word)):
        for j in range(n):
            if possible[i][j]:
                if (j + 1 not in delta.keys()) or (word[i] not in delta[j + 1].keys()):
                    continue
                for way in delta[j + 1][word[i]]:
                    possible[i + 1][way - 1] = True

    for term in terminals:
        if possible[len(word)][term - 1]:
            return "Accepts"
 
    return "Rejects"
 
 
with open("problem2.out", "w") as f:
    f.write(solve())
    f.close()