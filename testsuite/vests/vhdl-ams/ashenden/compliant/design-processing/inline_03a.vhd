
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

entity bottom is
  port ( terminal Tb : electrical; -- ... );
  -- not in book
         terminal Tz : electrical );
  -- end not in book
end entity bottom;

-- end code from book


architecture bottom_arch of bottom is

  -- code from book

  quantity -- ...
           i_b1 through Tb to Tz; -- ...;
  quantity -- ...
           i_b2 through Tb to Tz; -- ...;
  quantity -- ...
           i_b3 through Tz to Tb; -- ... to Tb;
  quantity -- ...
           i_b4 through Tz to Tb; -- ... to Tb;

  -- end code from book

begin

  assert
  -- code from book
  Tb'contribution = ( i_b1 + i_b2 ) - ( i_b3 + i_b4 )
  -- end code from book
  ;

end architecture bottom_arch;



library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity other_ent is
  port ( terminal Tx, Tz : electrical );
end entity other_ent;


architecture other_arch of other_ent is
begin
end architecture other_arch;



library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_03a is

end entity inline_03a;


architecture test of inline_03a is

  terminal Ty, Tb, Tx : electrical;

  -- code from book

  terminal T : electrical;
  quantity -- ...
           i_t1, i_t2 through T to Ty; -- ...;
  quantity -- ...
           i_t3 through Ty to T; -- ... to T;
  -- ...

  -- end code from book

begin

  -- code from book

  comp1 : entity work.bottom(bottom_arch)
    port map ( Tb => T, -- ... );
    -- not in book
               Tz => Ty );
    -- end not in book
  
  comp2 : entity work.other_ent(other_arch)
    port map ( Tx => T, -- ... );
    -- not in book
               Tz => Ty );
    -- end not in book
 
  -- end code from book


  assert
  -- code from book
    T'contribution = ( i_t1 + i_t2 ) - ( i_t3 ) + ( Tb'contribution + Tx'contribution )
  -- end code from book
  ;

end architecture test;
