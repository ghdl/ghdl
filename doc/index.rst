.. include:: shields.inc

.. only:: html

   |SHIELD:gh-logo| |SHIELD:gitter| |SHIELD:code-lic| |SHIELD:doc-lic| |SHIELD:travis-ci| |SHIELD:appveyor| |SHIELD:release|

------------------------------------

GHDL Documentation
##################

This manual is the user and reference manual for GHDL. It does not contain an
introduction to VHDL. Thus, the reader should have at least a basic knowledge
of VHDL. A good knowledge of VHDL language reference manual (usually called
LRM) is a plus.

.. only:: html

   News
   ****

   20.12.2017 - A new GitHub organization was created.
   ===================================================

   A new GitHub organization is created and the main repo is moved from github.com/tgingold/ghdl to
   github.com/ghdl/ghdl. Old refs will continue working, because permanent redirects are set up. However, we suggest
   every contributor to update the remote URLs in their local clones.

   14.12.2017 - `GHDL 0.35 was released <https://github.com/ghdl/ghdl/milestone/3?closed=1>`_.
   ====================================

   15.08.2017 - `GHDL 0.34 was released <https://github.com/ghdl/ghdl/milestone/1?closed=1>`_.
   ====================================

   23.10.2015 - GHDL 0.33 was released.
   ====================================

.. only:: latex

   .. rubric:: 20.12.2017 - GHDL 0.35 was released.

   .. rubric:: 15.08.2017 - GHDL 0.34 was released.

   .. rubric:: 23.10.2015 - GHDL 0.33 was released.

.. |docdate| date:: %b %d, %Y - %H:%M

.. container:: lastdocbuilddate

   This document was generated on |docdate|.

.. toctree::
   :caption: Introduction
   :hidden:

   about
   contribute
   licenses

.. raw:: latex

   \part{GHDL usage}

.. toctree::
   :caption: GHDL usage
   :hidden:

   using/QuickStartGuide
   using/InvokingGHDL
   using/Simulation

.. raw:: latex

   \part{Getting GHDL}

.. toctree::
   :caption: Getting GHDL
   :hidden:

   getting/Releases
   Building GHDL <building/Building>
   building/PrecompileVendorPrimitives

.. raw:: latex

   \part{References}

.. toctree::
   :caption: Implementation References
   :hidden:

   references/CommandReference
   references/CodingStyle
   references/ImplementationOfVHDL
   references/ImplementationOfVITAL

.. raw:: latex

   \part{Appendix}

.. toctree::
   :caption: Appendix
   :hidden:

   appendix/Roadmap
   appendix/Meta
   genindex
