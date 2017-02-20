.. _INTRO:Contributing:

Contributing
############

.. include:: ../shields.txt

The first step might be to use GHDL and explore it's possibilities in an own project. If you are new to VHDL, see the :ref:`Quick Start Guide <USING:QuickStart>` for an introduction. Furthermore, we encourage you to read :ref:`Invoking GHDL <USING:Invoking>`, where the most commonly used options are explained. You can also check the complete :ref:`Command Reference <REF:Command>`.

If you are more familiar with GHDL, you might start asking yourself how it works internally. Then, you migh find :ref:`Implementation of VHDL <REF:ImplVHDL>` and :ref:`Implementation of VITAL <REF:ImplVITAL>` interesting. 

While using GHDL, you might find flaws, such as bugs, missing features, typos in the documentation or topics which are still not covered. In order to improve GHDL, we welcome bugs report and suggestions for any aspect of GHDL. So, please report them so that we are aware!

Either if it's a bug or an enhancement, have a look at the |SHIELD:issues-open| and |SHIELD:issues-closed| to see if someone already told us about it. You might find a solution there. To get a broader view, you can also check the :ref:`Roadmap <CHANGE>`. Then, you can reach us through various ways:

- |SHIELD:gitter|
- Open a |SHIELD:issues-new|
- Fork, modify and create a Pull Request on |SHIELD:issues-pr| |SHIELD:issues-pr-closed|
- Suscribe to the mailing-list |SHIELD:mailing|

The indications below shall help you choose which one to take.

improve doc
examples


If you have an interresting project, please send us feedback or get listed on our :doc:`Who uses GHDL?` page.


Related interesting projects
==============
  
Asking for enhancements
==============

  .. _reporting_bugs:
  
Reporting bugs
==============

	If you cannot compile, please report the gcc version, GNAT version and gcc source version.
	- Minimum-(non)-Working-Example (MWE)
`How To Ask Questions The Smart Way <www.catb.org/~esr/faqs/smart-questions.html>`_

If the compiler crashes, this is a bug. Reliable tools never crash.

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

.. TODO::

  - Pull Requests (PRs)
	- Check Building -> GHDL -> Directory Structure]
	- Beware that some commit messages can `automatically close <https://help.github.com/articles/closing-issues-via-commit-messages/>`_ PRs]
	