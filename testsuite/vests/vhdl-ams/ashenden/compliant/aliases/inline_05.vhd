
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

package system_types is

  -- code from book

  type system_status is (idle, active, overloaded);

  -- end code from book

end package system_types;




entity inline_05 is

end entity inline_05;


----------------------------------------------------------------


architecture test of inline_05 is

  -- code from book

  alias status_type is work.system_types.system_status;

  -- end code from book

begin


  process_2_b : process is

    variable status : status_type := idle;

  begin
    wait for 10 ns;
    status := active;
    wait for 10 ns;
    status := overloaded;

    wait;
  end process process_2_b;


end architecture test;
