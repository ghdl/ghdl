#!/usr/bin/env python
import sys

def extract(out):
    # Skip until the first line
    while (1):
        l = sys.stdin.readline()
        if l == '':
            return False
        if l == '.. code-block:: VHDL\n':
            break

    # Write example
    while (1):
        l = sys.stdin.readline()
        if l[0] == '\n':
            out.write(l)
        elif len(l) >= 2 and l[:2] == '  ':
            out.write(l[2:])
        else:
            break

    return True

for f in sys.argv[1:]:
    print("Extracting {}...".format(f))
    with open(f, "w") as out:
        if not extract(out):
            sys.exit(1)
sys.exit(0)
