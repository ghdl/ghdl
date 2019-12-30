
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
                        
entity inline_10a is

end entity inline_10a;


architecture test of inline_10a is

  constant R : real := 10_000.0;
  constant R1 : real := 10_000.0;
  constant R2 : real := 10_000.0;

  -- code from book

  nature electrical_bus is
    record
      strobe: electrical;
      databus : electrical_vector(0 to 7);
    end record;

  -- end code from book

begin

  block_1 : block is

    -- code from book

    terminal bus_end1, bus_end2 : electrical_bus;
    quantity bus_v across bus_i through bus_end1 to bus_end2;

    -- end code from book

  begin

    -- code from book

    bus_v == bus_i * R;

    -- end code from book

  end block block_1;


  block_2 : block is

    terminal bus_end1, bus_end2 : electrical_bus;
    quantity bus_v across bus_i through bus_end1 to bus_end2;

  begin

    -- code from book

    bus_v.strobe == bus_i.strobe * R;
    bus_v.databus(0) == bus_i.databus(0) * R;
    bus_v.databus(1) == bus_i.databus(1) * R;
    --  ...
    -- not in book
    bus_v.databus(2) == bus_i.databus(2) * R;
    bus_v.databus(3) == bus_i.databus(3) * R;
    bus_v.databus(4) == bus_i.databus(4) * R;
    bus_v.databus(5) == bus_i.databus(5) * R;
    bus_v.databus(6) == bus_i.databus(6) * R;
    -- end not in book
    bus_v.databus(7) == bus_i.databus(7) * R;

    -- end code from book

  end block block_2;


  block_3 : block is

    terminal p, m : electrical;
    quantity v across i through p to m;

  begin

    -- code from book

    v == i * R;

    -- end code from book

  end block block_3;


  block_4 : block is

    terminal p, m : electrical;
    quantity v across i through p to m;

  begin

    -- code from book

    v / R == i;

    -- end code from book

  end block block_4;


  block_5 : block is

    terminal bus_end1, bus_end2 : electrical_bus;
    quantity bus_v across bus_i through bus_end1 to bus_end2;

  begin

    -- code from book

    bus_v.strobe == bus_i.strobe * R;
    bus_v.databus(0) == bus_i.databus(0) * R;

    -- end code from book

    bus_v.databus(1) == bus_i.databus(1) * R;
    bus_v.databus(2) == bus_i.databus(2) * R;
    bus_v.databus(3) == bus_i.databus(3) * R;
    bus_v.databus(4) == bus_i.databus(4) * R;
    bus_v.databus(5) == bus_i.databus(5) * R;
    bus_v.databus(6) == bus_i.databus(6) * R;
    bus_v.databus(7) == bus_i.databus(7) * R;

  end block block_5;


  block_6 : block is

    terminal p1, m1, p2, m2 : electrical;
    quantity v1 across i1 through p1 to m1;
    quantity v2 across i2 through p2 to m2;

  begin

    -- code from book

    i1 * R1 == i2 * R2; -- illegal

    -- end code from book

  end block block_6;


  block_7 : block is

    terminal p1, m1, p2, m2 : electrical;
    quantity v1 across i1 through p1 to m1;
    quantity v2 across i2 through p2 to m2;

  begin

    -- code from book

    i1 * R1 == i2 * R2 tolerance "current_tolerance";

    -- end code from book

  end block block_7;


  block_8 : block is

    terminal p1, m1, p2, m2 : electrical;
    quantity v1 across i1 through p1 to m1;
    quantity v2 across i2 through p2 to m2;

  begin

    -- code from book

    i1 * R1 == i2 * R2 tolerance i2'tolerance;

    -- end code from book

  end block block_8;


  block_9 : block is

    terminal p, m : electrical;
    quantity v across i through p to m;

  begin

    -- code from book

    v == i * R tolerance i'tolerance;

    -- end code from book

  end block block_9;

end architecture test;
