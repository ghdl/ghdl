import os
import sys
from string import join
from vunit import VUnit



# Create VUnit instance by parsing command line arguments
vu = VUnit.from_argv()
vu.add_com()
lib = vu.add_library("lib")
lib.add_source_files("*.vhd")

vu.main()