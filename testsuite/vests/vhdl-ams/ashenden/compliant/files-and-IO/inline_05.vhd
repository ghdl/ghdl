
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

entity inline_05 is

end entity inline_05;


----------------------------------------------------------------


architecture test of inline_05 is

  type log_file is file of string;

  -- code from book:

  file log_info : log_file open write_mode is "logfile";

  -- end of code from book

begin


  process is
  begin
    write(log_info, string'("AAAA"));
    wait for 1 ns;
    write(log_info, string'("BBBB"));
    wait;
  end process;


  process is
  begin
    write(log_info, string'("CCCC"));
    wait for 1 ns;
    write(log_info, string'("DDDD"));
    wait;
  end process;


end architecture test;	  
