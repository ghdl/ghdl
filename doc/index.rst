.. include:: shields.inc

.. only:: html

  |SHIELD:gh-logo| |SHIELD:gitter| |SHIELD:code-lic| |SHIELD:doc-lic| |br| |SHIELD:travis-ci| |SHIELD:appveyor| |SHIELD:release|

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

   15.08.2017 - GHDL 0.34 was released.
   ====================================

   23.10.2015 - GHDL 0.33 was released.
   ====================================

.. only:: latex

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
