import sys
import random

n=1024
if len(sys.argv) > 1:
    n = int(sys.argv[1])

sys.stdout.write("package data_pkg is\n")
sys.stdout.write("  type word_vector is array (natural range <>) of\n")
sys.stdout.write("      Natural;\n")
sys.stdout.write("\n")
sys.stdout.write("  constant data : word_vector := (\n")
for i in range(n):
    sys.stdout.write("    ")
    for j in range(4):
        sys.stdout.write('16#{:08x}#, '.format(random.getrandbits(31)))
    sys.stdout.write("\n")
sys.stdout.write('    16#{:08x}#\n'.format(random.getrandbits(31)))
sys.stdout.write("    );\n")
sys.stdout.write("end data_pkg;\n")
