
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
-- $Id: ch_20_ch_20_04.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

package ch_20_04 is

  attribute cell_name : string;

end package ch_20_04;



entity flipflop is

end entity flipflop;



use work.ch_20_04.all;

-- code from book:

architecture std_cell of flipflop is

  attribute cell_name of std_cell : architecture is "DFF_SR_QQNN";

  -- . . .       -- other declarations

begin
  -- . . .
end architecture std_cell;

-- end of code from book



-- code from book:

package model_utilities is

  attribute optimize : string;
  attribute optimize of model_utilities : package is "level_4";

  -- . . .

end package model_utilities;

-- end of code from book
