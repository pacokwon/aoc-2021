import itertools

dp = {}
def step(p1, p2, s1, s2):
    if s1 >= 21:
        return (1, 0)
    if s2 >= 21:
        return (0, 1)

    if (p1, p2, s1, s2) in dp:
        return dp[(p1, p2, s1, s2)]

    res = (0, 0)
    for d1, d2, d3 in itertools.product([1, 2, 3], [1, 2, 3], [1, 2, 3]):
        np1 = (p1 + d1 + d2 + d3) % 10
        ns1 = s1 + np1 + 1
        r2, r1 = step(p2, np1, s2, ns1)
        res = (res[0] + r1, res[1] + r2)

    dp[(p1, p2, s1, s2)] = res
    return res

p1 = 10 - 1
p2 = 6 - 1

print(step(p1, p2, 0, 0))
