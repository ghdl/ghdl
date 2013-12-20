
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
                        
-- code from book

entity battery is
  port ( terminal plus, minus : electrical );
end entity battery;

architecture wrong of battery is
  constant v_nominal : real := 9.0;
  quantity v across plus to minus;
begin
  v == v_nominal;
end architecture wrong;

--

architecture correct of battery is
  constant v_nominal : real := 9.0;
  quantity v across i through plus to minus;
begin
  v == v_nominal;
end architecture correct;

-- end code from book

                        

library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_04a is

end entity inline_04a;


architecture test of inline_04a is

  signal clamp : bit;
  quantity v1, v2 : real;

begin

  -- code from book

  if clamp = '1' use
    v1 == 5.0;
    v2 == 0.0;
  else
    v1 == v2;
  end use;

  -- end code from book

end architecture test;
                        
