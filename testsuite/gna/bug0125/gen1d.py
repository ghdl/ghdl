#!/usr/bin/env python3
# Generate a big aggregate

import sys
import random

out = sys.stdout


depth = 1024
width = 24

out.write("""
package repro is
  type array2d is array(0 to {}) of bit_vector({} downto 0);

  constant cst : array2d :=
  (
""".format(depth - 1, width - 1))

fmt = 'b"{{:0{}b}}"'.format(width)

for i in range(depth):
    if i != 0:
        out.write(',\n')
    out.write ('   ' + fmt.format(random.randint(0, 1<<width)));
out.write("""
   );
end repro;
""")
