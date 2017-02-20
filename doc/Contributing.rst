.. include:: shields.txt
.. _INTRO:Contributing:

Contributing
############

The first step might be to use GHDL and explore it's possibilities in an own project. If you are new to VHDL, see the :ref:`USING:QuickStart` for an introduction. Furthermore, we encourage you to read :ref:`USING:Invoking`, where the most commonly used options are explained. You can also check the complete :ref:`REF:Command`.

If you are more familiar with GHDL, you might start asking yourself how it works internally. Then, you migh find :ref:`Implementation of VHDL <REF:ImplVHDL>` and :ref:`Implementation of VITAL <REF:ImplVITAL>` interesting. 

While using GHDL, you might find flaws, such as bugs, missing features, typos in the documentation or topics which are still not covered. In order to improve GHDL, we welcome bug reports, suggestions and contributions for any aspect of GHDL. Either if it's a bug or an enhancement, have a look at the |SHIELD:issues-open| and |SHIELD:issues-closed| to see if someone already told us about it. You might find a solution there. To get a broader view, you can also check the :ref:`Roadmap <CHANGE>`.

If you found no information on your topic, please, report so that we are aware! You can reach us through various ways: |SHIELD:gitter|, open a |SHIELD:issues-new| and/or suscribe to |SHIELD:mailing|

.. HINT:: Since the development of GHDL started fifteen years ago, multiple platforms have been used as a support for both distribution and getting feedback. However, the development is now centralized in |SHIELD:gh-logo|.

	Therefore it is suggested to use the channels listed above instead of `gna.org/bugs/?=group=ghdl <https://gna.org/bugs/?group=ghdl>`_ or `sourceforge.net/p/ghdl-updates/tickets <https://sourceforge.net/p/ghdl-updates/tickets/>`_ . Still, these are checked from time to time.

.. TIP::
	`How To Ask Questions The Smart Way <www.catb.org/~esr/faqs/smart-questions.html>`_
	
.. _reporting_bugs:
  
Reporting bugs
==============

.. TIP::
	* If the compiler crashes, this is a bug. Reliable tools never crash.
	* If the compiler emits an error message for a perfectly valid input or does not emit an error message for an invalid input, this may be a bug.
	* If the executable created from your VHDL sources crashes, this may be a bug at runtime or the code itself may be wrong. However, since VHDL has a notion of pointers, an erroneous VHDL program (using invalid pointers for example) may crash.
	* If a compiler message is not clear enough, please tell us. The error messages can be improved, but we have not enough experience with them.

Please, report issues of this kind through |SHIELD:issues-new|, as this allows us to categorize issues into groups and assign developers to them. You can track the issue’s state and see how it’s getting solved.

.. IMPORTANT::
  To make it easier, please elaborate a `Minimum (non) Working Example` (`MWE <https://en.wikipedia.org/wiki/Minimal_Working_Example>`_) prior to sending the report, so that the possible bug source is isolated. Shall the MWE compile and run, it is a good idea to make it look like a test and make an assert statement should finish the execution; the severity level `note` indicates success, while a severity level `failure` indicates failure. 

  Then, please include enough information for the maintainers to reproduce the problem. This includes:

  - Operating system and version of GHDL (you can get it with :samp:`ghdl --version`).
  - Whether you have built GHDL from sources (provide short SHA of the used commit) or used the binary distribution (teel which realease/tag).

     - If you cannot compile, please report which compiler you are using and the version.
   
  - Content of the input files which make the MWE
  - Description of the problem:

     - Comment explaining whether the MWE should compile or not; if yes, whether or not is should run until the assertion.
     - What you expected to happen and what you actually get. If you know the LRM well enough, please specify the paragraph which might be not well implemented.
     - Samples of any log.
     - Anything else that you think would be helpful.

.. NOTE:: If you don't know the LRM, be aware that an issue claimed as bug report may be rejected because there is no bug according to it. GHDL aims at implementing VHDL as defined in `IEEE 1076 <http://ieeexplore.ieee.org/document/4772740/>`_. However, some other tools allow constructs which do not fully follow the standard revisions. Therefore, comparisons with other VHDL is not a solid argument. Some of them are supported by GHDL (see :ref:`IEEE_library_pitfalls`), but any such enhancement will have very low priority.

.. _requesting_enhancements:
	
Requesting enhancements
==============

|SHIELD:issues-new| |SHIELD:gitter| |SHIELD:mailing|

All enhancements and feature requests are welcome. Please `open a new issue <https://github.com/tgingold/ghdl/issues/new>`_ to report any, so you can track the request's status and implementation. Depending on the complexity of the request, you may want to `chat on Gitter <https://gitter.im/ghdl/ghdl1>`_, to polish it before opening an issue. If you don't have a `GitHub <https://github.com>`_ user account, you can reach the developers through the `mailing-list <https://mail.gna.org/listinfo/ghdl-discuss/>`_ .

Improving the documentation
==============

If you found a mistake in the documentation, please send a comment. If you didn't understand some parts of this manual, please tell us. English is not our mother tongue, so this documentation may not be well-written.

Likewise, rewriting part of the documentation or missing content (such as, examples) is a good way to improve it. Since it automatically is built from `reStructuredText` and `Markdown` sources, you can fork, modify and request the maintainers to pull your copy. See :ref:`pull_request`.

.. _pull_request:

Fork, modify and pull-request
==============

.. TIP::
  - Before starting any modification, you might want to have a look at |SHIELD:issues-pr| and |SHIELD:issues-pr-closed|, to check which other contributions are being made or have been made. If you observe that the modifications you are about to start might conflict with any other, please |SHIELD:gitter| or open a |SHIELD:issues-new| to coordinate.
  - See section :ref:`'BUILD:dir_structure'` to faster find the location of the sources you need to modify, and/or to know where to place new ones.

Contibuting source code/documentation via `Git <https://git-scm.com/>`_ is very easy. Although we don't provide direct write access to our repositories, the project is hosted at GitHub, which follows a fork, edit and pull-request `flow <https://help.github.com/articles/github-flow/>`_ . That is:

1. Make a copy (`fork <https://help.github.com/articles/fork-a-repo/>`_) of the project.
2. Do the changes you wish (edit, add, rename, move and/or delete).
3. When you think that the changes are ready to be merged, you notify the maintainers by opening a `Pull Request <https://help.github.com/articles/creating-a-pull-request/>`_ (PR).
4. The maintainers will review the proposed changes and will reply in the corresponding thread if any further modification is required. If so, you can keep adding commits to the same branch, and the PR will be automatically updated.
5. Last, the maintainers will merge your branch. You will be notified, the PR will be closed, and you'll be allowed to delete the branch, if you want.

.. TIP::
	* It is recommended to read `A successful Git branching model <http://nvie.com/posts/a-successful-git-branching-model/>`_ for a reference on how maintainers expect to handle multiple branches. However, our actual model is not as exhaustive as explained there.
	* Some commit messages can `automatically close <https://help.github.com/articles/closing-issues-via-commit-messages/>`_ issues. This is a very useful feature, which you are not required to use. However beware that using `fix` anywhere in the commit message can have side effects. If you closed any issue unexpectedly, just reply to it (even if it's closed) so that maintainers can check it.

Related interesting projects
==============

If you have an interesting project, please send us feedback or get listed on our :ref:`INTRO:WHO` page.  
