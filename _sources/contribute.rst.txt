.. program:: ghdl

.. only:: html

   .. exec::
      from helpers import createShields
      createShields('shields')

.. _INTRO:Contributing:

Contributing
############

As in many other free and open source projects, there are many areas requiring different skills where contributions to GHDL
are welcome. The first step might be to use GHDL and explore its possibilities in your own project. If you are new to VHDL,
see the :ref:`USING:QuickStart:Simulation` for an introduction. Furthermore, we encourage you to read :ref:`USING:Invoking`,
where the most commonly used options are explained. You can also check the complete :ref:`REF:Command`.

If you are more familiar with GHDL, you might start asking yourself how it works internally. If so, you might find
:ref:`Implementation of VHDL <REF:ImplVHDL>` and :ref:`Implementation of VITAL <REF:ImplVITAL>` interesting.

While using GHDL, you might find flaws, such as bugs, missing features, typos in the documentation, or topics which still are
not covered. In order to improve GHDL, we welcome bug reports, suggestions, and contributions for any aspect of
GHDL. Whether it's a bug or an enhancement, have a look at the |SHIELD:issues-open| and |SHIELD:issues-closed| to see
if someone already told us about it. You might find a solution there.

Ideas for future work, enhancements, documentation, and internship programs are shown in the `GitHub wiki <https://github.com/ghdl/ghdl/wiki>`__.

If you found no information on your topic, please, report so that we are aware! You can reach us through various ways:
|SHIELD:gitter| or open a |SHIELD:issues-new|.

.. HINT::
   * Since the development of GHDL started in 2002, multiple platforms have been used as a support for both distribution
     and getting feedback. However, the development is now centralized in `github.com/ghdl <https://github.com/ghdl>`__.
   * `How To Ask Questions The Smart Way <www.catb.org/~esr/faqs/smart-questions.html>`_

.. _reporting_bugs:

Reporting bugs
==============

* If the compiler crashes, this is a bug. Reliable tools never crash.
* If the compiler emits an error message for a perfectly valid input or does not emit an error message for an invalid
  input, this may be a bug.
* If the executable created from your VHDL sources crashes, this may be a bug at runtime or the code itself may be
  wrong. Since VHDL has a notion of pointers, an erroneous VHDL program (using invalid pointers for example) may crash.
* If a compiler message is not clear enough, please tell us. The error messages can be improved, but we do not have
  enough experience with them.
* It is suggested to test synthesis features with :option:`--synth`, before processing the design with :ref:`Synth:plugin`.

Please, report issues through |SHIELD:bug-report|, as this allows us to categorize issues into groups and to assign developers
to them. You can track the state and see how it's getting solved.

.. IMPORTANT::
   As suggested in the bug report template, please elaborate a `Minimal (non) Working Example` (:wikipedia:`MWE <Minimal_Working_Example>`)
   prior to sending the report, so that the possible bug source is isolated. Should it fulfill the format requirements of
   `issue-runner <https://github.com/umarcor/issue-runner>`_, you would be able to test your bug with the latest GHDL version.
   Please do so in order to ensure that the bug is not solved already.

   Also, please include enough information in the bug report, for the maintainers to reproduce the problem. The template
   includes:

   * Operating system and version of GHDL (you can get it with :samp:`ghdl version` and :samp:`ghdl hash`).
   * Whether you have built GHDL from sources (provide short SHA of the used commit) or used the binary distribution
     (note which release/tag); if you cannot compile, please report which compiler you are using and the version.
   * Content of the input files which comprise the MWE.
   * Description of the problem:

     * Comment explaining whether the MWE should compile or not; if yes, whether it should run until the assertion.
     * What you expect to happen and what you actually get. If you know the LRM well enough, please specify which paragraph
       might not be implemented well.
     * Samples of any log.
     * Anything else that you think would be helpful.

.. NOTE::
   If you don't know the LRM, be aware that an issue claimed as a bug report may be rejected because there is no bug
   according to it. GHDL aims at implementing VHDL as defined in `IEEE 1076 <http://ieeexplore.ieee.org/document/4772740/>`__.
   However, some other tools allow constructs which do not fully follow the standard revisions. Therefore, comparisons
   with other VHDL variants is not a solid argument. Some of them are supported by GHDL (see :ref:`IEEE_library_pitfalls`),
   but any such enhancement will have very low priority.

.. _requesting_enhancements:

Requesting enhancements
=======================

|SHIELD:feature-request| |SHIELD:gitter|

All enhancements and feature requests are welcome. Please `open a new issue <https://github.com/ghdl/ghdl/issues/new>`_
to report any, so you can track the status and implementation. Depending on the complexity of the request,
you may want to `chat on Gitter <https://gitter.im/ghdl/ghdl1>`_, for polishing it before opening an issue.

Improving the documentation
===========================

If you found a mistake in the documentation, please send a comment. If you didn't understand some parts of this manual,
please tell us. English is not our mother tongue, so this documentation may not be well-written.

Likewise, rewriting part of the documentation or missing content (such as examples) is a good way to improve it. Since
it is built automatically from `reStructuredText`, you can fork, modify and push. The documentation will be shown
in the GitHub Pages site of your fork: ``https://USERNAME.github.io/ghdl``. When you are done, request the maintainers
to pull your copy. See :ref:`pull_request`.

.. _pull_request:

Fork, modify and pull-request
=============================

.. TIP::
   * Before starting any modification, you might want to have a look at |SHIELD:issues-pr| and |SHIELD:issues-pr-closed|,
     to check which other contributions are being made or have been made. If you observe that the modifications you are
     about to start might conflict with any other, please |SHIELD:gitter| or open a |SHIELD:new-pr| to coordinate.
   * See section :ref:`BUILD:dir_structure` to faster find the location of the sources you need to modify, and/or to know
     where to place new ones.

Contributing source code/documentation is done through `git <https://git-scm.com/>`__. Although we don't provide direct
write access to our repositories, the project is hosted at GitHub, which follows a fork, edit and pull-request
`flow <https://help.github.com/articles/github-flow/>`__ . That is:

1. Make a copy (`fork <https://help.github.com/articles/fork-a-repo/>`_) of the project.
2. Do the changes you wish (edit, add, rename, move and/or delete).
3. When you think that the changes are ready to be merged, notify the maintainers by opening a `Pull Request <https://help.github.com/articles/creating-a-pull-request/>`__ (PR).
4. The maintainers will review the proposed changes and will reply in the corresponding thread if any further modification
   is required. If so, you can keep adding commits to the same branch, and the PR will be automatically updated.
5. Last, maintainers will merge your branch. You will be notified, the PR will be closed, and you'll be allowed to
   delete the branch, if you want.

.. TIP::
  * It is recommended to read `A successful Git branching model <http://nvie.com/posts/a-successful-git-branching-model/>`_
    for a reference on how maintainers expect to handle multiple branches. However, our actual model is not as exhaustive
    as explained there.
  * Some commit messages can `automatically close <https://help.github.com/articles/closing-issues-via-commit-messages/>`_
    issues. This is a very useful feature, which you are not required to use. However beware that using `fix` anywhere
    in the commit message can have side effects. If you closed any issue unexpectedly, just reply to it (even if it's
    closed) so that maintainers can check it.
  * It is recommended to read :ref:`DEV:Style` before contributing modifications to Ada sources.

Related interesting projects
============================

If you have an interesting project, please send us feedback or get listed on our :ref:`INTRO:WHO` page.
