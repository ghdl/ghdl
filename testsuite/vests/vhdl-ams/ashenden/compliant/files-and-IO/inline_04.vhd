
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

entity inline_04 is

end entity inline_04;


----------------------------------------------------------------


architecture test of inline_04 is
begin


  process is

    type data_file_type is file of character;
    variable ch : character;

    -- code from book:

    procedure write_to_file is
      file data_file : data_file_type open write_mode is "datafile";
    begin
      -- . . .
      -- not in book
      write(data_file, ch);
      -- end not in book
    end procedure write_to_file;

    -- end of code from book

  begin
    ch := 'A';
    write_to_file;
    ch := 'B';
    write_to_file;
    ch := 'C';
    write_to_file;

    wait;
  end process;


end architecture test;
