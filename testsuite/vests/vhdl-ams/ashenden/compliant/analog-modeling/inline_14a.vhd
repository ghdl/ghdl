
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_14a is

end entity inline_14a;


architecture test of inline_14a is

  terminal p : electrical;
  quantity v across i through p;
  constant R : resistance := 10_000.0;

  type modeling_mode_type is (ideal, non_ideal);
  constant modeling_mode : modeling_mode_type := ideal;

begin

  -- code from book

  if modeling_mode = ideal use
    v == i * R;
  else
    null; -- still need to include resistor with thermal effects!
  end use;

  -- end code from book

end architecture test;
