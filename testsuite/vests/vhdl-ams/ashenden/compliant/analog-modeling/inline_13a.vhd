
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
                        
entity inline_13a is

end entity inline_13a;


architecture test of inline_13a is

  -- code from book

  quantity v : voltage;
  -- ...

  -- end code from book

begin

  -- code from book

  if v'above(0.0) and not v'above(0.6) use
    -- ...
  elsif v'above(0.6) and not v'above(2.7) use
    -- ...
  else
    -- ...
  end use;

  --

  case v use -- illegal
    when 0.0 to 0.6 =>
      -- ...;
    when 0.6 to 2.7 =>
      --...;
    when others =>
      --...;
  end case;

  -- end code from book

end architecture test;
