
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

library ieee_proposed; use ieee_proposed.electrical_systems.all;

entity pwl_load is
  generic ( load_enable : boolean := true;
            res_init : resistance;
            res1 : resistance;                  
            t1 : time;
            res2 : resistance;
            t2 : time );
  port ( terminal p1, p2 : electrical );
end entity pwl_load;

----------------------------------------------------------------

architecture ideal of pwl_load is

  quantity v across i through p1 to p2;
  signal res_signal : resistance := res_init;

begin

  load_present : if load_enable generate

    if domain = quiescent_domain or domain = frequency_domain use
       v == i * res_init; 
    else 
       v == i * res_signal'ramp(1.0e-6, 1.0e-6);
    end use;

    create_event : process is
    begin
      wait for t1;
      res_signal <= res1;
      wait for t2 - t1;
      res_signal <= res2;
      wait;
    end process create_event;

  end generate load_present;

  load_absent : if not load_enable generate

    i == 0.0;

  end generate load_absent;

end architecture ideal;
