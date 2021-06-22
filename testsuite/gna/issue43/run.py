from vunit import VUnit

vu = VUnit.from_argv()
vu.add_com()
vu.add_library("lib").add_source_files("*.vhd")
vu.main()
