import sys
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

acc = lines[0]
for (idx, line) in enumerate(lines):
    if idx == 0: continue
    acc = add_snails(acc, line)

psnail(acc)
print(magnitude(acc))

