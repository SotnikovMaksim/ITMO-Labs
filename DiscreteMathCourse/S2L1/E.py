from typing import *
 

def getDelta(m, f):
    delta: Dict[int, Dict[str, Set[int]]] = dict()
    for i in range(m):
        req = f.readline().split()
        fr = int(req[0]) - 1
        to = int(req[1]) - 1
        s = req[-1]
        if fr not in delta.keys():
            delta[fr] = dict()
            delta[fr][s] = set()
        elif s not in delta[fr].keys():
            delta[fr][s] = set()
        delta[fr][s].add(to)
    f.close()
    return delta

 
def solve():
    f = open("problem5.in", "r")
    n, m, k, l = [int(i) for i in f.readline().split()]
    terminal = [int(i) - 1 for i in f.readline().split()]
    if len(terminal) == 0:
        return 0
    MOD = 10 ** 9 + 7
    delta = getDelta(m, f)
    n, terminal, delta = nfa2dfa(terminal, delta)
    dp = [[[False, 0] for i in range(n)] for j in range(l + 1)]
    dp[0][0] = [True, 1]
    for i in range(l):
        for j in range(n):
            if dp[i][j][0]:
                if j not in delta.keys():
                    continue
                for s in delta[j].keys():
                    dp[i + 1][delta[j][s]][0] = True
                    dp[i + 1][delta[j][s]][1] += dp[i][j][1]

    ans = 0
    for term in terminal:
        ans += dp[l][term][1]
 
    return ans % MOD
 
def nfa2dfa(terminals, delta):
    queue: List[Set[int]] = []
    states: List[Set[int]] = []
    start: Set[int] = set()
    start.add(0)
    queue.append(start)
    trans: Dict[int, Dict[str, Set[int]]] = dict()
    states.append(start)
    keys: List[Set[int]] = []
    while len(queue) != 0:
        pd: Set[int] = queue.pop(0)
        for i in range(26):
            q: Set[int] = set()
            symbol: str = chr(i + 97)
            for stateSet in pd:
                if stateSet in delta.keys():
                    if symbol in delta[stateSet].keys():
                        t = delta[stateSet][symbol]
                        q = q.union(delta[stateSet][symbol])
                        way: Dict[str, Set[int]] = dict()
                        way[symbol] = q
                        if pd not in keys:
                            trans[len(keys)] = way
                            keys.append(pd)
                        else:
                            trans[keys.index(pd)][symbol] = q
            if q not in states:
                queue.append(q)
                states.append(q)
    newTerms: List[Set[int]] = []
    for stateSet in states:
        for state in stateSet:
            if state in terminals:
                newTerms.append(stateSet)
    n: int = len(states)
    frozenQueue: List[frozenset[int]] = []
    for s in states:
        frozenQueue.append(frozenset(s))
    ind: int = 0
    rename: Dict[frozenset[int], int] = dict()
    terminals: List[int] = []
    newDelta: Dict[int, Dict[str, int]] = dict()
    for state in frozenQueue:
        rename[state] = ind
        newDelta[ind] = dict()
        ind += 1
 
    for state in rename.keys():
        ind = rename[state]
        if state in newTerms:
            terminals.append(ind)
        if state in keys:
            for symbol in trans[keys.index(state)].keys():
                nfaStates: Set[int] = trans[keys.index(state)][symbol]
                num: int = rename[frozenset(nfaStates)]
                newDelta[ind][symbol] = num

    return n, terminals, newDelta
 
ans = solve()
with open("problem5.out", "w") as f:
    f.write(str(ans))
    f.close()


# DEBUG FUNCTION
def dump(a):
    for i in a:
        print(*i)