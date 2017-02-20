.. include:: shields.txt

|SHIELD:gh-logo| |SHIELD:gitter| |SHIELD:code-lic| |SHIELD:doc-lic| |br| |SHIELD:travis-ci| |SHIELD:appveyor| |SHIELD:tag| |SHIELD:release|

------------------------------------ 
	  
GHDL Documentation
########
	  
This manual is the user and reference manual for GHDL. It does not contain an introduction to VHDL. Thus, the reader should have at least a basic knowledge of VHDL. A good knowledge of VHDL language reference manual (usually called LRM) is a plus.
	  
.. only:: html

   News
   ****
   
   23.10.2015 - GHDL 0.33 was released.
   ====================================
   
.. only:: latex

   .. rubric:: 23.10.2015 - GHDL 0.33 was released.
   
Lorem ipsum dolor sit amet...

.. TODO:

  - http://ghdl.free.fr
  - In `doc/conf.py` add a command to copy `./COPYING.md` to `doc/License.md`, and `NEWS.md` to `doc/changelog/index.md`.
						 
.. |docdate| date:: %b %d, %Y - %H:%M

.. container:: lastdocbuilddate

		This document was generated on |docdate|.
   
.. toctree::
   :caption: Introduction
   :hidden:
   
   About
   Contributing
   Licenses
   
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
   getting/Docker
   getting/Building
   getting/VendorPrimitives
   
.. raw:: latex

   \part{References}
   
.. toctree::
   :caption: Implementation references
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
   
   changelog/Roadmap
   changelog/index
   genindex
   