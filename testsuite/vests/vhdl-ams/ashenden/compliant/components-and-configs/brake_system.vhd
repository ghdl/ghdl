
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

-- not in book

use work.automotive_valve_defs.all;

entity brake_system is
end entity brake_system;

-- end not in book



architecture structure of brake_system is

  use work.automotive_valve_defs.all;

  -- ...  -- declarations of other components, terminals, etc

  -- not in book
  terminal master_reservoir, brake_line : valve_fluidic;
  terminal brake_pedal : valve_translational;
  -- end not in book

begin
  
  pedal_valve : component automotive_valve
    port map ( p1 => master_reservoir,
               p2 => brake_line,
               control => brake_pedal );
    
  -- ...  -- other component instances
      
end architecture structure;
