#!/usr/bin/env python3
# Generate a big aggregate

import sys
import random

out = sys.stdout


depth = 1024
width = 6

out.write("""
package repro is
  type array2d is array(0 to {}, 0 to {}) of integer;

  constant cst : array2d :=
  (
""".format(depth - 1, width - 1))

for i in range(depth):
    if i != 0:
        out.write(',\n')
    out.write ('   (')
    for j in range(width):
        if j != 0:
            out.write(', ')
        out.write('{}'.format(random.randint(0, 1<<31)))
    out.write(')')
out.write("""
   );
end repro;
""")
