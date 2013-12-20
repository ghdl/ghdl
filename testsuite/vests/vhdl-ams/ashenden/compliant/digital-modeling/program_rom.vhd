
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

-- not in book

library ieee;  use ieee.std_logic_1164.all;

-- end not in book


entity program_ROM is
  port ( address : in std_ulogic_vector(14 downto 0);
         data : out std_ulogic_vector(7 downto 0);
         enable : in std_ulogic );

  subtype instruction_byte is bit_vector(7 downto 0);
  type program_array is array (0 to 2**14 - 1) of instruction_byte;
  constant program : program_array
    := ( X"32", X"3F", X"03",  -- LDA  $3F03
         X"71", X"23",         -- BLT    $23
         -- not in book
         others => X"00"
         -- end not in book
         -- . . .
       );

end entity program_ROM;
