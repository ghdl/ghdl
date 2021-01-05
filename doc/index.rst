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

   31.01.2021 - GHDL v1.0.0rc1 was tagged
   ======================================

   * Python bindings were overhauled and renamed to ``pyGHDL``. Three modules are included: ``libghdl``, ``lsp`` and ``dom``.

     * The utility scripts in the codebase were moved into subdir ``scripts``: CI, binding generation, vendors, etc.

   * Repository `ghdl/extended-tests <https://github.com/ghdl/extended-tests>`__ was created for testing `vendors` build scripts.

   * The logo was updated (org, ghdl/ghdl, ghdl/docker and ghdl/ghdl-cosim).

   * Assets are not added to releases or pre-releases anymore. Users should use package managers or nightly assets.

   21.05.2020 - Nightly build assets available
   ===========================================

   * After each successful CI run of branch ``master``, packages are published as assets of pre-release `nightly <https://github.com/ghdl/ghdl/releases/tag/nightly>`__.
   * GitHub Action `ghdl/setup-ghdl-ci <https://github.com/ghdl/setup-ghdl-ci>`__ was created, to allow easy installation of
     nightly GHDL assets in GitHub Actions workflows.

   09.05.2020 - New repositories and a wiki were created
   =====================================================

   * The plugin for Yosys was moved from `tgingold/ghdlsynth-beta <https://github.com/tgingold/ghdlsynth-beta>`__ to
     `ghdl/ghdl-yosys-plugin <https://github.com/ghdl/ghdl-yosys-plugin>`__.
   * Repository `ghdl/ghdl-cosim <https://github.com/ghdl/ghdl-cosim>`__ was created. It contains documentation and code
     examples related to VHPIDIRECT, VPI and SystemC. See :ref:`COSIM` and `Previous work and future ideas <https://github.com/ghdl/ghdl-cosim/issues/1>`__.
   * A `Wiki <https://github.com/ghdl/ghdl/wiki>`__ was created. The roadmap and ideas for documentation and internship
     programs were moved there. If you want to contribute anyhow, `have a look <https://github.com/ghdl/ghdl/wiki>`__!

   28.02.2020 - `GHDL v0.37 was released <https://github.com/ghdl/ghdl/milestone/8?closed=1>`__
   ============================================================================================

   The major changes are:

      * Experimental support of synthesis (either with --synth or with
	the Yosys plugin).
      * Fixes and improved support of vhdl 2008.
      * Last version that supports the Mentor variation of
	std_logic_arith.  The Synopsys one is still available.

   03.03.2019 - `GHDL v0.36 was released <https://github.com/ghdl/ghdl/milestone/7?closed=1>`__
   ============================================================================================

   23.02.2019 - GHDL v0.36-rc1 was released
   ========================================

   The major improvements are:

      * more support of unbounded arrays and records
      * support of UVVM and Vunit

   29.11.2018 - GHDL 20181129 was released
   =======================================

   20.12.2017 - A new GitHub organization was created
   ==================================================

   A new GitHub organization is created and the main repo is moved from `github.com/tgingold/ghdl <https://github.com/tgingold/ghdl>`__ to
   `github.com/ghdl/ghdl <https://github.com/ghdl/ghdl>`__. Old refs will continue working, because permanent redirects are set up. However, we suggest
   every contributor to update the remote URLs in their local clones.

   14.12.2017 - `GHDL 0.35 was released <https://github.com/ghdl/ghdl/milestone/3?closed=1>`__
   ===========================================================================================

   15.08.2017 - `GHDL 0.34 was released <https://github.com/ghdl/ghdl/milestone/1?closed=1>`__
   ===========================================================================================

   23.10.2015 - GHDL 0.33 was released
   ===================================

.. only:: latex

   .. rubric:: 31.01.2021 - GHDL v1.0.0rc1 was tagged.

   .. rubric:: 21.05.2020 - Nightly build assets available.

   .. rubric:: 09.05.2020 - New repositories and a wiki were created.

   .. rubric:: 28.02.2020 - GHDL v0.37 was released.

   .. rubric:: 03.03.2019 - GHDL v0.36 was released.

   .. rubric:: 23.02.2019 - GHDL v0.36-rc1 was released.

   .. rubric:: 29.11.2018 - GHDL 20181129 was released.

   .. rubric:: 20.12.2017 - A new GitHub organization was created.

   .. rubric:: 14.12.2017 - GHDL 0.35 was released.

   .. rubric:: 15.08.2017 - GHDL 0.34 was released.

   .. rubric:: 23.10.2015 - GHDL 0.33 was released.

.. toctree::
   :caption: Introduction
   :hidden:

   about
   getting
   contribute
   licenses

.. raw:: latex

   \part{Usage}

.. toctree::
   :caption: GHDL usage
   :hidden:

   quick_start/index
   using/InvokingGHDL
   using/Simulation
   using/Synthesis
   using/CommandReference
   using/ImplementationOfVHDL
   using/ImplementationOfVITAL

.. raw:: latex

   \part{Development}

.. toctree::
   :caption: Development
   :hidden:

   development/Directories
   Building GHDL <development/building/index>
   Python Interfaces <pyGHDL/pyGHDL>
   development/Debugging
   development/CodingStyle
   development/Scripts

.. raw:: latex

   \part{Internals}

.. toctree::
   :caption: Internals
   :hidden:

   internals/Overview
   internals/Frontend
   internals/AST
   internals/RTI
   gnatdoc/index

.. raw:: latex

   \part{Index}

.. toctree::
   :caption: Index
   :hidden:

   genindex
   py-modindex
