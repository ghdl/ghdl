
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

entity inline_09 is

end entity inline_09;


----------------------------------------------------------------


architecture test of inline_09 is

begin

  process is

    type value_cell;

    type value_ptr is access value_cell;

    type value_cell is record
        value : bit_vector(0 to 3);
        next_cell : value_ptr;
      end record value_cell;

    variable value_list, cell_to_be_deleted : value_ptr;

  begin
    value_list := new value_cell'( B"1000", value_list );
    value_list := new value_cell'( B"0010", value_list );
    value_list := new value_cell'( B"0000", value_list );

    -- code from book:

    cell_to_be_deleted := value_list;
    value_list := value_list.next_cell;
    deallocate(cell_to_be_deleted);

    while value_list /= null loop
      cell_to_be_deleted := value_list;
      value_list := value_list.next_cell;
      deallocate(cell_to_be_deleted);
    end loop;

    -- end of code from book

    wait;
  end process;

end architecture test;
