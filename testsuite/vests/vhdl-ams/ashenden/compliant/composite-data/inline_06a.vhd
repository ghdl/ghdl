
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

entity inline_06a is

end entity inline_06a;


----------------------------------------------------------------


architecture test of inline_06a is

  -- code from book:

  subtype resistance is real tolerance "default_resistance";
  type resistance_array is array (1 to 4) of resistance;
  quantity resistances : resistance_array := (10.0, 20.0, 50.0, 75.0);

  -- end of code from book


begin


  block_1_f : block is

    -- code from book:

    quantity resistances : resistance_array := (1 => 10.0, 2 => 20.0, 3 => 50.0, 4 => 75.0);

    -- end of code from book

  begin
  end block block_1_f;


end architecture test;
