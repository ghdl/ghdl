
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

-- analyze into resource library graphics

package graphics_pkg is

  attribute graphic_symbol : string;
  attribute graphic_style : string;

end package graphics_pkg;



-- code from book

library ieee;  use ieee.std_logic_1164.all;
library graphics;

package gate_components is

  use graphics.graphics_pkg.graphic_symbol,
    graphics.graphics_pkg.graphic_style;

  component and2 is
    generic ( prop_delay : delay_length );
    port ( a, b : in std_logic;  y : out std_logic );
  end component and2;

  attribute graphic_symbol of and2 : component is "and2";
  attribute graphic_style of and2 : component is "color:default, weight:bold";

  -- . . .

end package gate_components;

-- end code from book
