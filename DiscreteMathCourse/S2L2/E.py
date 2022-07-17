from collections import defaultdict
import time
 
test = defaultdict(list)
 
 
def getDelta(f, m):
    delta = defaultdict(list)
    for _ in range(m):
        req = f.readline().strip().split()
        delta[req[0]].append(list(req[2])) if len(req) == 3 else delta[req[0]].append([''])
    return delta
 
 
def dfs(state):
    # print(state)
    if state.islower():
        return [[state]]
    if state not in delta:
        return []
    if state in visited and visited[state]:
        return []
    ans = []
 
 
    visited[state] = True
    for way in delta[state]:
        if len(way) == 1 and way[0] != state:
            ans += dfs(way[0])
        else:
            ans += [way]
    visited[state] = False
    return ans
 
 
def deleteChainRules():
    for state, rules in delta.copy().items():
        for rule in rules.copy():
            if rule[0] != '' and len(rule) == 1 and rule[0][0].isupper():
                delta[state].pop(delta[state].index(rule))
                trans = dfs(rule[0])
                for t in trans:
                    if t not in delta[state]:
                        delta[state].append(t)
 
 
def preprocessing() -> None:
    for state in delta.copy():
        for way in delta[state]:
            for s in range(len(way)):
                if way[s].islower():
                    newState = 'N' + way[s]
                    if newState not in delta:
                        delta[newState] = [list(way[s])]
                    way[s] = newState
 
 
def deleteEpsilonRules() -> None:
    global s
    generative = dict()
    eps = set()
 
    for state in delta:
        if [''] in delta[state]:
            eps.add(state)
        else:
            for way in delta[state]:
                if all([i.isupper() for i in way]):
                    if state not in generative:
                        generative[state] = []
                    generative[state].append(way)
 
    size = len(eps)
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
 
    for state in delta:
        for way in delta[state]:
            if len(way) == 2:
                if way[0] in eps:
                    if way[1:] not in delta[state]:
                        delta[state].append(way[1:])
                if way[1] in eps:
                    if way[:1] not in delta[state]:
                        delta[state].append(way[:1])
                if way[0] in eps and way[1] in eps:
                    if [''] not in delta[state]:
                        delta[state].append([''])
            elif len(way) == 1 and way[0] in eps:
                delta[state].append([''])
 
    if s in eps:
        if [''] in delta[s]:
            delta[s].pop(delta[s].index(['']))
        else:
            for way in delta[s]:
                if all([i in eps for i in way]):
                    delta[s].pop(delta[s].index(way))
                    break
        delta[s * 2] = [[''], [s]]
        s = s * 2
 
 
def deleteLongRules() -> None:
    for state, rules in delta.copy().items():
        for rule in rules.copy():
            if len(rule) > 2:
                stateName = ''.join(rule)
                st = stateName + '1'
                delta[state].append([rule[0], st])
                for i in range(2, len(rule) - 1):
                    delta[st] = [[rule[i - 1], stateName + str(i)]]
                    st = stateName + str(i)
                delta[st] = [rule[-2:]]
                delta[state].pop(delta[state].index(rule))
 
 
startTime = time.time()
f = open('cf.in', 'r')
n, s = f.readline().strip().split()
n = int(n)
visited = dict()
 
delta = getDelta(f, n)
 
preprocessing()
 
deleteLongRules()
 
deleteEpsilonRules()
 
deleteChainRules()
 
for state, rules in delta.copy().items():
    if state != s:
        delta[state] = [rule for rule in rules if rule != ['']]
        if not delta[state]:
            delta.pop(state, None)
 
word = f.readline().strip()
f.close()
length = len(word)
t = [[set() for _ in range(length + 1)] for _ in range(length + 1)]
 
start = s
cykTime = time.time()
for s in range(1, length + 1):
    for state, rules in delta.items():
        for rule in rules:
            if len(rule) == 1 and rule[0] == word[s - 1]:
                t[1][s].add(state)
 
 
for l in range(2, length + 1):
    for s in range(1, length - l + 2):
        for p in range(1, l):
            for state, rules in delta.items():
                for rule in rules:
                    if len(rule) == 2:
                        if rule[0] in t[p][s] and rule[1] in t[l - p][s + p]:
                            t[l][s].add(state)
                
 
with open('cf.out', 'w') as f:
    if start not in t[length][1]:
        f.write('no')
    else:
        f.write('yes')