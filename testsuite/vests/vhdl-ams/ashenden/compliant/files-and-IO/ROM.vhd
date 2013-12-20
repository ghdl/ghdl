
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

library ieee;  use ieee.std_logic_1164.all;

entity ROM is
  generic ( load_file_name : string );
  port ( sel : in std_logic;
         address : in std_logic_vector;
         data : inout std_logic_vector );
end entity ROM;

--------------------------------------------------

architecture behavioral of ROM is

begin

  behavior : process is

    subtype word is std_logic_vector(0 to data'length - 1);
    type storage_array is
      array (natural range 0 to 2**address'length - 1) of word;
    variable storage : storage_array;
    variable index : natural;
    -- . . .    -- other declarations

    type load_file_type is file of word;
    file load_file : load_file_type open read_mode is load_file_name;

  begin

    -- load ROM contents from load_file
    index := 0;
    while not endfile(load_file) loop
      read(load_file, storage(index));
      index := index + 1;
    end loop;

    -- respond to ROM accesses
    loop
      -- . . .
    end loop;

  end process behavior;

end architecture behavioral;
