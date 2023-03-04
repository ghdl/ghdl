.. exec::
   from helpers import createShields
   createShields()

GHDL
####

.. raw:: html

   <p style="text-align: center;">
     <a title="GitHub Repository" href="https://github.com/ghdl/ghdl"><img src="https://img.shields.io/badge/-ghdl/ghdl-323131.svg?longCache=true&style=flat-square&logo=github"></a><!--
     -->
     <a title="Talk to us on Gitter" href="https://gitter.im/ghdl1/Lobby"><img src="https://img.shields.io/badge/chat-on%20gitter-4db797.svg?longCache=true&style=flat-square&logo=gitter&logoColor=fff"></a><!--
     -->
     <a title="GNU General Public License 2" href="Licenses.html"><img src="https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?longCache=true&style=flat-square&label=license&logo=gnu"></a><!--
     -->
     <a title="Creative Commons Attribution-ShareAlike 4.0" href="Licenses.html"><img src="https://img.shields.io/badge/doc%20license-CC--BY--SA--4.0-bf7600.svg?longCache=true&style=flat-square&logo=Creative%20Commons&logoColor=fff"></a><!--
     -->
     <a title="Latest release" href="http://ghdl.rtfd.io"><img src="https://img.shields.io/github/release/ghdl/ghdl.svg?longCache=true&style=flat-square&label=latest%20release"></a><!--
     -->
   </p>

   <p style="text-align: center; font-weight: 700; font-size: 125%; margin: 24px 0;">
   GHDL: free and open-source analyzer, compiler, simulator and (experimental) synthesizer for VHDL
   </p>

.. only:: html

   News
   ****

   2022.02.28 - `GHDL v2.0 was released <https://github.com/ghdl/ghdl/milestone/10?closed=1>`__
   ============================================================================================

   2021.02.02 - `GHDL v1.0 was released <https://github.com/ghdl/ghdl/milestone/9?closed=1>`__
   ============================================================================================

   2021.01.31 - GHDL v1.0.0rc1 was tagged
   ======================================

   * Python bindings were overhauled and renamed to ``pyGHDL``. Three modules are included: ``libghdl``, ``lsp`` and ``dom``.

     * The utility scripts in the codebase were moved into subdir ``scripts``: CI, binding generation, vendors, etc.

   * Repository `ghdl/extended-tests <https://github.com/ghdl/extended-tests>`__ was created for testing `vendors` build scripts.

   * The logo was updated (org, ghdl/ghdl, ghdl/docker and ghdl/ghdl-cosim).

   * Assets are not added to releases or pre-releases anymore. Users should use package managers or nightly assets.

   2020.05.21 - Nightly build assets available
   ===========================================

   * After each successful CI run of branch ``master``, packages are published as assets of pre-release `nightly <https://github.com/ghdl/ghdl/releases/tag/nightly>`__.
   * GitHub Action `ghdl/setup-ghdl-ci <https://github.com/ghdl/setup-ghdl-ci>`__ was created, to allow easy installation of
     nightly GHDL assets in GitHub Actions workflows.

   2020.05.09 - New repositories and a wiki were created
   =====================================================

   * The plugin for Yosys was moved from `tgingold/ghdlsynth-beta <https://github.com/tgingold/ghdlsynth-beta>`__ to
     `ghdl/ghdl-yosys-plugin <https://github.com/ghdl/ghdl-yosys-plugin>`__.
   * Repository `ghdl/ghdl-cosim <https://github.com/ghdl/ghdl-cosim>`__ was created. It contains documentation and code
     examples related to VHPIDIRECT, VPI and SystemC. See :ref:`COSIM` and `Previous work and future ideas <https://github.com/ghdl/ghdl-cosim/issues/1>`__.
   * A `Wiki <https://github.com/ghdl/ghdl/wiki>`__ was created. The roadmap and ideas for documentation and internship
     programs were moved there. If you want to contribute anyhow, `have a look <https://github.com/ghdl/ghdl/wiki>`__!

   2020.02.28 - `GHDL v0.37 was released <https://github.com/ghdl/ghdl/milestone/8?closed=1>`__
   ============================================================================================

   The major changes are:

   * Experimental support of synthesis (either with --synth or with the Yosys plugin).
   * Fixes and improved support of vhdl 2008.
   * Last version that supports the Mentor variation of std_logic_arith.
     The Synopsys one is still available.

   2019.03.03 - `GHDL v0.36 was released <https://github.com/ghdl/ghdl/milestone/7?closed=1>`__
   ============================================================================================

   2019.02.23 - GHDL v0.36-rc1 was released
   ========================================

   The major improvements are:

   * more support of unbounded arrays and records
   * support of UVVM and Vunit

   2018.11.29 - GHDL 20181129 was released
   =======================================

   2017.12.20 - A new GitHub organization was created
   ==================================================

   A new GitHub organization is created and the main repo is moved from `github.com/tgingold/ghdl <https://github.com/tgingold/ghdl>`__ to
   `github.com/ghdl/ghdl <https://github.com/ghdl/ghdl>`__. Old refs will continue working, because permanent redirects are set up. However, we suggest
   every contributor to update the remote URLs in their local clones.

   2017.12.14 - `GHDL 0.35 was released <https://github.com/ghdl/ghdl/milestone/3?closed=1>`__
   ===========================================================================================

   2017.08.15 - `GHDL 0.34 was released <https://github.com/ghdl/ghdl/milestone/1?closed=1>`__
   ===========================================================================================

   2015.10.23 - GHDL 0.33 was released
   ===================================

.. only:: latex

   .. rubric:: 2022.02.28 - GHDL v2.0 was released.

   .. rubric:: 2021.02.02 - GHDL v1.0 was released.

   .. rubric:: 2021.01.31 - GHDL v1.0.0rc1 was tagged.

   .. rubric:: 2020.05.21 - Nightly build assets available.

   .. rubric:: 2020.05.09 - New repositories and a wiki were created.

   .. rubric:: 2020.02.28 - GHDL v0.37 was released.

   .. rubric:: 2019.03.03 - GHDL v0.36 was released.

   .. rubric:: 2019.02.23 - GHDL v0.36-rc1 was released.

   .. rubric:: 2018.11.29 - GHDL 20181129 was released.

   .. rubric:: 2017.12.20 - A new GitHub organization was created.

   .. rubric:: 2017.12.14 - GHDL 0.35 was released.

   .. rubric:: 2017.08.15 - GHDL 0.34 was released.

   .. rubric:: 2015.10.23 - GHDL 0.33 was released.

.. include:: toc.rst
