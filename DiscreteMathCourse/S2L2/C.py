from typing import *
 
 
def getDelta(f, m) -> Dict[str, Set[str]]:
    delta: Dict[str, Set[str]] = dict()
    delta[s] = set()
    for i in range(m):
        req = f.readline().strip().split()
        if req[0] not in delta:
            delta[req[0]] = set()
        if len(req) == 2:
            delta[req[0]].add('')
        else:
            fill(req[2], delta)
            delta[req[0]].add(req[2])
    return delta
 
 
def fill(word: str, delta: Dict[str, Set[str]]):
    for s in word:
        if s.isupper():
            if s not in delta:
                delta[s] = set()
 
 
# def dfs1(s):
#     if (not s.isupper()) or (alpha[s]):
#         return
#     alpha[s] = True
#     if s not in delta:
#         return
#     for way in delta[s]:
#         for a in way:
#             if a.isupper() and not alpha[a]:
#                 dfs1(a)
 
f = open("useless.in", "r")
n, s = f.readline().split()
n = int(n)
delta = getDelta(f, n)
edges: Dict[str, Set[str]] = delta.copy()
 
 
generative: Set[str] = set()
for state in delta.copy():
    for way in delta[state]:
        if all([not i.isupper() for i in way]):
            generative.add(state)
            delta.pop(state, None)
 
# S -> aaD
size: int = 0
while True:
    for state in delta.copy():
        for way in delta[state]:
            if all([(not i.isupper() or (i.isupper() and i in generative)) for i in way]):
                generative.add(state)
                delta.pop(state, None)
 
    if size == len(generative):
        break
    size = len(generative)
 
possible: Set[str] = {s}
size = 1
while True:
    for state in edges.copy():
        if state in possible:
            for way in edges[state]:
                if any([i in way for i in delta]):
                    continue
                for s in way:
                    if s.isupper():
                        possible.add(s)
            edges.pop(state, None)
    if size == len(possible):
        break
    size = len(possible)
 
 
with open("useless.out", "w") as f:
    ans: Set[str] = set()
    for state in edges:
        ans.add(state)
    for state in delta:
        ans.add(state)
    f.write(' '.join(sorted(list(ans))))