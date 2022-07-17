def getDelta(m, f):
    delta = dict()
    alphabet = set()
    for i in range(m):
        fr, to, s = f.readline().split()
        if int(fr) in delta:
            delta[int(fr)][s] = int(to)
        else:
            delta[int(fr)] = dict()
            delta[int(fr)][s] = int(to)
        alphabet.add(s)
 
    return delta, alphabet
 
 
def solve():
    f = open("problem1.in", "r")
    word = f.readline().strip()
    n, m, k = [int(i) for i in f.readline().split()]
    terminals = [int(i) for i in f.readline().split()]
 
 
    if len(terminals) == 0:
        print("Rejects")
    delta, alphabet = getDelta(m, f)
    f.close()
 
    v = 1
    for i in range(len(word)):
        if (v not in delta.keys()) or (word[i] not in delta[v].keys()):
            return "Rejects"
        v = delta[v][word[i]]
 
    if v in terminals:
        return "Accepts"
    return "Rejects"
 
 
with open("problem1.out", "w") as f:
    f.write(solve())
    f.close()