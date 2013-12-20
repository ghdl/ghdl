
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
-- $Id: ch_05_fg_05_06.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity mux2 is
  port ( a, b, sel : in bit;
         z : out bit );
end entity mux2;

--------------------------------------------------

architecture behavioral of mux2 is

  constant prop_delay : time := 2 ns;

begin

  slick_mux : process is
  begin
    case sel is
      when '0' =>
        z <= a after prop_delay;
        wait on sel, a;
      when '1' =>
        z <= b after prop_delay;
        wait on sel, b;
    end case;
  end process slick_mux;

end architecture behavioral;
