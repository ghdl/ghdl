.. _CHANGE:Roadmap:

Meta
############

* Python snippet for Sphinx's `conf.py` to extract the current version number from Git (latest tag name). 

	* :ghdlsharp:`200`

* Reference :samp:`genindex.html` from the navigation bar.

	* :ghdlsharp:`200`

* Create "parts" (LaTeX terminology / chapter headlines) in navigation bar.

	* :ghdlsharp:`200`
	
* Ubuntu uses `dash` instead of `bash` when a shell script is run. As a result, some functionalities, such as arrays like :samp:`array[1]`, are not supported. Therefore, build scripts in `dist/linux` should not use those functionalities unless they are sourced in a `bash` shell. That is, :file:`tavis-ci.sh` uses arrays, since it is sourced in the Travis CI machine. But :file:`docker-buildtest.sh` and :file:`buildtest.sh` do not use any. The same applies to the scripts in `testsuite`.