
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

library ieee_proposed;
use ieee_proposed.electrical_systems.all;
use ieee_proposed.mechanical_systems.all;
                        
entity inline_09a is

end entity inline_09a;


architecture test of inline_09a is


  constant R : real := 1.0e3;
  constant k : real := 10.0;

  -- code from book

  terminal p, m : electrical;
  quantity v across i through p to m;

  --

  terminal node1, node2 : translational;
  quantity d across f through node1 to node2;

  -- end code from book

begin

  -- code from book

  v == i * R;

  --

  f == d * k;

  -- end code from book

end architecture test;
