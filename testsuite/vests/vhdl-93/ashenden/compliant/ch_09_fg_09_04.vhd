
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
-- $Id: ch_09_fg_09_04.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

entity DMA_controller is
end entity DMA_controller;

-- end not in book



architecture behavioral of DMA_controller is

  use work.DMA_controller_types_and_utilities.all;

begin

  behavior : process is

                       variable address_reg0, address_reg1 : address;
                     variable count_reg0, count_reg1 : word;
                     -- . . .

  begin
    -- . . .
    address_reg0 := address_reg0 + X"0000_0004";
    -- . . .
  end process behavior;

end architecture behavioral;
