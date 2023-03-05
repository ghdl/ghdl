.. _INTRO:Copyrights:

Copyrights | Licenses
#####################

- The GHDL front-end package ``std.textio``, and the runtime library ``GRT`` are given under :ref:`LIC:GPLv2`.
- The documentation is given under :ref:`LIC:CC-BY-SA`.

.. WARNING::
   As a consequence of the runtime copyright, you are not allowed to distribute an executable produced by GHDL without allowing
   access to the VHDL sources. Please, send a comment (:ref:`requesting_enhancements`) if you don't like this policy.

The following packages are copyrighted by third parties (see corresponding sources for more information):

* These from library ``ieee`` are copyrighted by `Institute of Electrical and Electronics Engineers (IEEE) <https://www.ieee.org>`__:

  * Since December 2019, standard and IEEE libraries are distributed under Apache 2.0 and available at
    `opensource.ieee.org/vasg/Packages <https://opensource.ieee.org/vasg/Packages>`__. GHDL includes backports of those
    libraries for earlier revisions of the standard.
  * ``VITAL_Primitives``, ``VITAL_Timing`` and ``VITAL_Memory`` are not include in the open source distribution of IEEE (see `IEEE 1076.4 <http://ieeexplore.ieee.org/document/954750/>`__).

* The following sources may be used and distributed without restriction, provided that the copyright statements are not
  removed from the files and that any derivative work contains the copyright notice.

  * ``synopsys`` directory: ``std_logic_arith``, ``std_logic_signed``, ``std_logic_unsigned`` and ``std_logic_textio`` are
    copyrighted by `Synopsys, Inc. <https://www.synopsys.com/>`__
  * ``mentor`` directory: ``std_logic_arith`` is copyrighted by `Mentor Graphics <https://www.mentor.com>`__.

.. _LIC:GPLv2:

GNU GPLv2
=========

GHDL is copyright |copy| 2002 - 2023 Tristan Gingold.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but **WITHOUT ANY WARRANTY**; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License <https://www.gnu.org/licenses/old-licenses/gpl-2.0.html>`__ for more details.

.. _LIC:CC-BY-SA:

CC-BY-SA
========

This is a free documentation; you can redistribute it and/or modify it under the terms of the `Creative Commons Attribution-ShareAlike 4.0 <https://creativecommons.org/licenses/by-sa/4.0/>`__ license. You are free to **share** (copy and redistribute the material in any medium or format) and/or **adapt** (remix, transform, and build upon the material for any purpose, even commercially). We cannot revoke these freedoms as long as you follow the these terms:

- **Attribution**: you must provide the name of the creator and attribution parties (`more info <https://wiki.creativecommons.org/wiki/License_Versions#Detailed_attribution_comparison_chart>`__), a copyright notice, a license notice, a disclaimer notice, a link to the material, a link to the license and indicate if changes were made (see `marking guide <https://wiki.creativecommons.org/wiki/Best_practices_for_attribution#This_is_a_good_attribution_for_material_you_modified_slightly>`__ and `more info <https://wiki.creativecommons.org/wiki/License_Versions#Modifications_and_adaptations_must_be_marked_as_such>`__). You may do so in any reasonable manner, but not in any way that suggests we endorse you or your use.
- **ShareAlike**: if you remix, transform, or build upon the material, you must distribute your contributions under the same license as the original.
- **No additional restrictions**: you may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

See `CC-BY-SA-4.0 Legal Code <https://creativecommons.org/licenses/by-sa/4.0/legalcode.txt>`__ for more details.

.. _LIC:contributors:

List of Contributors
====================

============================= ===============================================================================================================
Contributor [#f1]_            Role
============================= ===============================================================================================================
Baggett, Jonas                signal selection
Bertram, Felix                VPI interface
Davis, Brian                  Windows Mcode builds
Drummond, Brian               GCC 4.8.2 update, OSVVM port, some bugfixes
Gingold, Tristan [#f2]_       **Sole author of GHDL as a whole**
Jensen, Adam                  FreeBSD builds
Koch, Markus                  vendor pre-compile script for Lattice (GNU/Linux)
Koontz, David                 Mac OSX builds, LRM compliance work, bugfix analyses
Lehmann, Patrick              Windows compile scripts, vendor library pre-compile scripts (win+lin), building in MinGW, AppVeyor integration.
Meißner, Torsten              Property Specification Language (PSL): docs, tests, synthesis support
Martinez-Corral, Unai [#f2]_  ghdl-cosim, setup-ghdl-ci, docs, docker/CI, MSYS2 packaging, building/testing on ARM, termux builds
van Rantwijk, Joris           Debian packaging
============================= ===============================================================================================================

.. only:: html

   .. exec::
      from helpers import createShields
      createShields('shields')

Only those who made substantial contributions are shown in the table above, but many others contributed with minor patches. You can find a list at |SHIELD:contributors|

With apologies to anyone who ought to be either on this table or in the GitHub contributor list, but isn't. Thanks also to all those who have reported bugs and support issues, and often patches and testcases to either the late gna! website or `sourceforge.net/p/ghdl-updates/tickets <https://sourceforge.net/p/ghdl-updates/tickets/>`__.

--------------------------------------------------------------------------------

.. container:: footnotes

   .. rubric:: Footnotes

   .. [#f1] In alphabetical order
   .. [#f2] Maintainer
