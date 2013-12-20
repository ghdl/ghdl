
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
-- $Id: ch_03_fg_03_07.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity SR_flipflop is
  port ( S, R : in bit;  Q : out bit );
end entity SR_flipflop;

architecture checking of SR_flipflop is
begin

  set_reset : process (S, R) is
  begin
    assert S = '1' nand R = '1';
    if S = '1' then
      Q <= '1';
    end if;
    if R = '1' then
      Q <= '0';
    end if;
  end process set_reset;

end architecture checking;
