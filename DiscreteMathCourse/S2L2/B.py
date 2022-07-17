from typing import *
 
 
def getDelta(f, n) -> (Dict[str, Set[str]], Dict[str, Set[str]], Set[str]):
    delta: Dict[str, Set[str]] = dict()
    generative: Dict[str, Set[str]] = dict()
    eps: Set[str] = set()
    for state in range(n):
        req: List[str] = f.readline().strip().split()
        if req[0] not in delta.keys():
            delta[req[0]] = set()
        if len(req) == 2:
            delta[req[0]].add('')
        else:
            delta[req[0]].add(req[2])
 
    for state in delta:
        if '' in delta[state]:
            eps.add(state)
        else:
            for way in delta[state]:
                if all([i.isupper() for i in way]):
                    if state not in generative:
                        generative[state] = set()
                    generative[state].add(way)
 
    return delta, generative, eps
 
 
f = open("epsilon.in", "r")
n, s = f.readline().split()
n = int(n)
delta, generative, eps = getDelta(f, n)
size: int = len(eps)
 
while True:
    for state in generative.copy():
        for way in generative[state]:
            if all([i in eps for i in way]):
                generative.pop(state, None)
                eps.add(state)
                break
 
    if len(eps) == size:
        break
    size = len(eps)
 
with open("epsilon.out", "w") as f:
    epsStates: List[str] = sorted(list(eps))
    f.write(" ".join(epsStates))