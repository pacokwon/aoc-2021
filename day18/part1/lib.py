INVALID_INDEX = -1

def is_num(c):
    return ord('0') <= ord(c) <= ord('9')

def snail2list(snail):
    i = 0
    length = len(snail)
    l = []
    while i < length:
        c = snail[i]
        if c == '[' or c == ']' or c == ',':
            l.append(c)
            i += 1
        else:
            acc = 0
            while is_num(snail[i]):
                acc = acc * 10 + ord(snail[i]) - ord('0')
                i += 1
            l.append(acc)

    return l

def psnail(snail):
    for c in snail:
        print(c, end='')
    print()

def search_left_pos(snail, start):
    index = start - 1
    while index >= 0:
        c = snail[index]
        if isinstance(c, int):
            return index
        index -= 1

    return INVALID_INDEX

def search_right_pos(snail, start):
    index = start + 1
    length = len(snail)
    while index < length:
        c = snail[index]
        if isinstance(c, int):
            return index
        index += 1

    return INVALID_INDEX

def list2int(l):
    acc = 0
    for n in l:
        acc = acc * 10 + int(n)

    return acc

def read_pair(snail, l, r):
    return snail[l + 1], snail[r - 1]

# [[[[[9,8],1],2],3],4]
def explode(snail):
    depth = 0

    length = len(snail)

    i = 0
    while i < length:
        c = snail[i]
        if c == '[':
            depth += 1
            if depth == 5:
                left_b = i
                break
        elif c == ']':
            depth -= 1

        i += 1

    if depth != 5:
        return (snail, False)

    # if input is correct, it must break
    j = i + 1
    while j < length:
        if snail[j] == ']':
            break
        j += 1
    right_b = j

    left = search_left_pos(snail, left_b)
    right = search_right_pos(snail, right_b)
    fst, snd = read_pair(snail, left_b, right_b)

    if left != INVALID_INDEX:
        snail[left] = snail[left] + fst

    if right != INVALID_INDEX:
        snail[right] = snail[right] + snd

    return (snail[:left_b] + [0] + snail[right_b + 1:], True)

def split(snail):
    length = len(snail)
    index = 0
    while index < length:
        c = snail[index]
        if isinstance(c, int) and c >= 10:
            return (snail[:index] + ['[', c // 2, ',', c - c // 2, ']'] + snail[index + 1:], True)
        index += 1
    return (snail, False)

def partition(snail):
    depth = 0
    length = len(snail)
    index = 0
    while index < length:
        c = snail[index]
        if c == '[':
            depth += 1
        elif c == ']':
            depth -= 1

        if depth == 1 and c == ',':
            return snail[1:index], snail[index+1:length-1]

        index += 1

    raise ValueError("Invalid Snail!")

def magnitude(snail):
    f, s = partition(snail)

    mf = f[0] if len(f) == 1 else magnitude(f)
    ms = s[0] if len(s) == 1 else magnitude(s)

    return 3 * mf + 2 * ms

# print(split(split(snail2list("[[[[0,7],4],[15,[0,13]]],[1,1]]"))))
