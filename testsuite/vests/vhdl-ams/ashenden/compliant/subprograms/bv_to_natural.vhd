
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

entity bv_to_natural is
end entity bv_to_natural;



architecture test of bv_to_natural is

  -- code from book

  function bv_to_natural ( bv : in bit_vector ) return natural is
    variable result : natural := 0;
  begin
    for index in bv'range loop
      result := result * 2 + bit'pos(bv(index));
    end loop;
    return result;
  end function bv_to_natural;

  -- end code from book

  signal data : bit_vector(0 to 7);
  constant address : bit_vector(0 to 3) := "0101";
  constant Taccess : delay_length := 80 ns;

begin

  tester : process is

    constant rom_size : natural := 8;
    constant word_size : natural := 8;

    -- code from book (in text)

    type rom_array is array (natural range 0 to rom_size-1)
                        of bit_vector(0 to word_size-1);
    variable rom_data : rom_array;

    -- end code from book

  begin

    rom_data := (X"00", X"01", X"02", X"03", X"04", X"05", X"06", X"07");

    -- code from book (in text)

    data <= rom_data ( bv_to_natural(address) ) after Taccess;

    -- end code from book

    wait;
  end process tester;

end architecture test;
