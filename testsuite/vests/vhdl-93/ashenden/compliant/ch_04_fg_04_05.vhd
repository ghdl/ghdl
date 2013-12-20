
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_04_fg_04_05.vhd,v 1.2 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book:
library ch4_pkgs;
use ch4_pkgs.pk_04_02.all;
-- end not in book:


entity byte_swap is
  port (input : in halfword;  output : out halfword);
end entity byte_swap;

--------------------------------------------------

architecture behavior of byte_swap is

begin

  swap : process (input)
  begin
    output(8 to 15) <= input(0 to 7);
    output(0 to 7) <= input(8 to 15);
  end process swap;

end architecture behavior;
