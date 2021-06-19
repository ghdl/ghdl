from sys import executable
from subprocess import check_call, STDOUT
from pathlib  import Path
from glob import glob
from pytest import mark

if __name__ == "__main__":
	print("ERROR: you called a testcase declaration file as an executable module.")
	print("Use: 'python -m unitest <testcase module>'")
	exit(1)


@mark.parametrize(
	"file",
	glob(str(Path(__file__).resolve().parent.parent.parent.parent / '**' / '*.vhdl'), recursive=True)
)
@mark.xfail
def test_AllVHDLSources(file):
	check_call([
		executable,
		str(Path(__file__).resolve().parent.parent.parent.parent / 'pyGHDL' / 'cli' / 'DOM.py'),
		file
	], stderr=STDOUT)
