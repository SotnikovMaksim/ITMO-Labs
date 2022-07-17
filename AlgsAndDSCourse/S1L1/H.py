def psp(s):
    o = ['(', '[', '{']
    c = [')', ']', '}']
    stack = []
    for i in range(len(s)):
        if s[i] in o:
            stack.append(s[i])
        else:
            if len(stack) > 0:
                if c.index(s[i]) == o.index(stack[-1]):
                    stack.pop()
                else:
                    return 'NO'
            else:
                return 'NO'
    if len(stack) == 0:
        return 'YES'
    return 'NO'


print(psp(input()))