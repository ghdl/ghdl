--  GHDL Run Time (GRT) - ghost declarations for ieee.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

--  This packages provides dummy declaration for main IEEE.STD_LOGIC_1164
--  type descriptors.
--  The package must not have elaboration code, since the actual type
--  descriptors are not writable (they are constant).  Making it preelaborated
--  is not enough, the variables must be initialized.  This current
--  implementation provides bad values; this is not a problem since they are
--  not read in grt.

package Grt.Shadow_Ieee is
   pragma Preelaborate (Grt.Shadow_Ieee);

   procedure Ieee_Std_Logic_1164_Resolved_RESOLV;
private
   pragma Export (C, Ieee_Std_Logic_1164_Resolved_RESOLV,
                  "ieee__std_logic_1164__resolved_RESOLV");
end Grt.Shadow_Ieee;
