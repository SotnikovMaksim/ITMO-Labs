from typing import *
 
 
def getDelta(m, f):
    delta: Dict[int, Dict[str, Set[int]]] = dict()
    for i in range(m):
        req = f.readline().strip().split()
        fr = ord(req[0].lower()) - 96
        if len(req[2]) > 1:
            s = req[2][0]
            to = ord(req[2][1].lower()) - 96
        else:
            s = req[2][0]
            to = 27
        if fr not in delta.keys():
            delta[fr] = dict()
            delta[fr][s] = set()
        if s not in delta[fr].keys():
            delta[fr][s] = set()
        delta[fr][s].add(to)
 
    return delta
 
 
def product(word):
    possible = [[False for _ in range(28)] for _ in range(len(word) + 1)]
    possible[0][ord(s.lower()) - 96] = True
    for i in range(len(word)):
        for j in range(27):
            if possible[i][j]:
                if (j not in delta.keys()) or (word[i] not in delta[j].keys()):
                    continue
                for way in delta[j][word[i]]:
                    possible[i + 1][way] = True
 
    if possible[len(word)][27]:
        return True
 
    return False
 
 
f = open("automaton.in", "r")
n, s = f.readline().split()
n: int = int(n)
delta: Dict[int, Dict[str, Set[int]]] = getDelta(n, f)
 
m = int(f.readline())
words = []
for i in range(m):
    words.append(f.readline().strip())
f.close()
 
with open("automaton.out", "w") as f:
    for word in words[:-1]:
        if product(word):
            f.write("yes\n")
        else:
            f.write("no\n")
    if product(words[-1]):
        f.write("yes")
    else:
        f.write("no")
    f.close()