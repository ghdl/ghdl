
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_07_fg_07_12.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_07_12 is
end entity fg_07_12;



architecture test of fg_07_12 is

  -- code from book

  procedure find_first_set ( v : in bit_vector;
                             found : out boolean;
                             first_set_index : out natural ) is
  begin
    for index in v'range loop
      if v(index) = '1' then
        found := true;
        first_set_index := index;
        return;
      end if;
    end loop;
    found := false;
  end procedure find_first_set;

  -- end code from book

begin

  stimulus : process is

                       -- code from book (in text)

                       variable int_req : bit_vector (7 downto 0);
                     variable top_priority : natural;
                     variable int_pending : boolean;
                     -- . . .

                     -- end code from book

                     constant block_count : natural := 16;

                     -- code from book (in text)

                     variable free_block_map : bit_vector(0 to block_count-1);
                     variable first_free_block : natural;
                     variable free_block_found : boolean;
                     -- . . .

                     -- end code from book

  begin
    int_req := "00010000";

    -- code from book (in text)

    find_first_set ( int_req, int_pending, top_priority );

    -- end code from book

    free_block_map := (others => '0');

    -- code from book (in text)

    find_first_set ( free_block_map, free_block_found, first_free_block );

    -- end code from book

    wait;
  end process stimulus;

end architecture test;
