
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

entity inline_03 is

end entity inline_03;


----------------------------------------------------------------


architecture test of inline_03 is
begin


  process is

    type element_type is (t1, t2, t3);

    type file_type is file of element_type;

    -- code from book:

    procedure write ( file f : file_type;  value : in element_type );

    -- end of code from book

    procedure write ( file f : file_type;  value : in element_type ) is
    begin
    end;

  begin
    wait;
  end process;


end architecture test;
