This manual is the user and reference manual for GHDL. It does not contain an introduction to VHDL. Thus, the reader should have at least a basic knowledge of VHDL. A good knowledge of VHDL language reference manual (usually called LRM) is a plus.

.. only:: html

   .. raw:: html
   
      <hr />

   .. image:: /_static/logos/GitHub-Mark-32px.png
      :scale: 60
      :target: https://www.github.com/tgingold/ghdl
      :alt: Source Code on GitHub
   .. image:: https://travis-ci.org/tgingold/ghdl.svg?branch=release
      :target: https://travis-ci.org/tgingold/ghdl
      :alt: Build status by Travis-CI
   .. image:: https://ci.appveyor.com/api/projects/status/r5dtv6amsppigpsp/branch/release?svg=true
      :target: https://ci.appveyor.com/project/Paebbels/poc/branch/release
      :alt: Build status by AppVeyor
   .. image:: https://badges.gitter.im/ghdl/ghdl1.svg
      :target: https://gitter.im/ghdl/ghdl1
      :alt: Join
      
   .. raw:: html
   
      <br />
      
   .. image:: https://img.shields.io/github/tag/tgingold/ghdl.svg?style=flat
      :alt: Latest tag
   .. image:: https://img.shields.io/github/release/tgingold/ghdl.svg?style=flat
      :target: https://github.com/tgingold/ghdl/releases
      :alt: Latest release
   .. image:: https://img.shields.io/github/license/tgingold/ghdl.svg?style=flat
      :target: License.html
      :alt: GNU General Public License 2
      
   .. raw:: html
   
      <hr />

.. only:: html

   News
   ****
   
   23.10.2015 - GHDL 0.33 was released.
   ====================================
   
.. only:: latex

   .. rubric:: 23.10.2015 - GHDL 0.33 was released.
   
Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,
no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit
amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut
labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam
et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata
sanctus est Lorem ipsum dolor sit amet

 
------------------------------------

.. |docdate| date:: %b %d, %Y - %H:%M

.. only:: html

   This document was generated on |docdate|.
   
.. toctree::
   :caption: Introduction
   :hidden:
   
   0_Intro/WhatIsVHDL
   0_Intro/WhatIsGHDL
   0_Intro/Contributing
   0_Intro/Copyrights
   License/gpl-2.0
   
.. raw:: latex

   \part{GHDL usage}
   
.. toctree::
   :caption: GHDL usage
   :hidden:
   
   1_Using/QuickStartGuide
   1_Using/InvokingGHDL
   1_Using/Simulation
   
.. raw:: latex

   \part{Getting GHDL}
   
.. toctree::
   :caption: Getting GHDL
   :hidden:
   
   2_Getting/Releases
   2_Getting/Docker
   3_Building/index
   3_Building/VendorPrimitives
   
.. raw:: latex

   \part{References}
   
.. toctree::
   :caption: Implementation references
   :hidden:
   
   4_References/CommandReference
   4_References/ImplementationOfVHDL
   4_References/ImplementationOfVITAL
   
.. raw:: latex

   \part{Appendix}
   
.. toctree::
   :caption: Appendix
   :hidden:
   
   X_ChangeLog/Roadmap
   X_ChangeLog/index
   genindex
   