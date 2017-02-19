.. _INTRO:Contributing:

Contributing
############

Despite all the testing and already reported `issues <https://github.com/tgingold/ghdl/issues>`_, you can find bugs
or propose enhancements.

  .. _reporting_bugs:

Asking for enhancements
==============
  
Reporting bugs
==============

In order to improve GHDL, we welcome bugs report and suggestions for
any aspect of GHDL.  Please create an issue on
https://github.com/tgingold/ghdl/issues

If the compiler crashes, this is a bug.  Reliable tools never crash.

If your compiled VHDL executable crashes, this may be a bug at
runtime or the code produced may be wrong.  However, since VHDL
has a notion of pointers, an erroneous VHDL program (using invalid
pointers for example) may crash.

If the compiler emits an error message for a perfectly valid input or
does not emit an error message for an invalid input, this may be a bug.
Please send the input file and what you expected.  If you know the LRM
well enough, please specify the paragraph which has not been well
implemented.  If you don't know the LRM, maybe your bug report will be
rejected simply because there is no bug.  In the latter case, it may be
difficult to discuss the issue; and comparisons with other VHDL tools
is not a very strong argument.

If a compiler message is not clear enough for you, please tell me.  The
error messages can be improved, but I have not enough experience with
them.

If you send a `VHDL` file producing a bug, it is a good idea to try
to make it as short as possible.  It is also a good idea to make it
looking like a test: write a comment which explains whether the file
should compile, and if yes, whether or not it should run successfully.
In the latter case, an assert statement should finish the test; the
severity level note indicates success, while a severity level failure
indicates failure.

For bug reports, please include enough information for the maintainers to
reproduce the problem. This includes:

* the version of `GHDL` (you can get it with :samp:`ghdl --version`).
* the operating system
* whether you have built `GHDL` from sources or used the binary
  distribution.
* the content of the input files
* a description of the problem and samples of any erroneous input
* anything else that you think would be helpful.

Documentation
==============

If you have found a mistake in the manual, please send a comment.  If
you have not understood some parts of this manual, please tell me.
English is not my mother tongue, so this manual may not be well-written.
Again, rewriting part of it is a good way to improve it.

---

@TODO:

- Reporting bugs
	- [1138: Issues, search first]
	- Minimum-(non)-Working-Example (MWE)
- Pull Requests (PRs)
	- [1138: check chapter 2 -> building -> GHDL -> directory structure]
	- [1138: beware that some commit messages can `automatically close <https://help.github.com/articles/closing-issues-via-commit-messages/>`_ PRs]