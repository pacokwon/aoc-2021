import sys
import itertools
from lib import snail2list, psnail, explode, split, magnitude

def progress(snail):
    snail, exploded = explode(snail)
    while exploded:
        snail, exploded = explode(snail)

    snail, splitted = split(snail)
    if not splitted:
        return snail

    return progress(snail)

def add_snails(snail1, snail2):
    added = ['['] + snail1 + [','] + snail2 + [']']
    added = progress(added)
    return added

lines = [snail2list(l[:len(l) - 1]) for l in sys.stdin]
total_count = len(lines)

perms = [(l1, l2) for l1, l2 in itertools.product(lines, lines) if l1 != l2]
sums = [magnitude(add_snails(l1, l2)) for l1, l2 in perms]
print(max(sums))
