
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

entity inline_10 is

end entity inline_10;


----------------------------------------------------------------


library ieee;  use ieee.std_logic_1164.std_ulogic;

architecture test of inline_10 is

  -- code from book:

  type std_ulogic_vector is array ( natural range <> ) of std_ulogic;
  
  --
  
  subtype std_ulogic_word is std_ulogic_vector(0 to 31);
  
  --
  
  signal csr_offset : std_ulogic_vector(2 downto 1);

  -- end of code from book

begin


  process_2_b : process is

    -- code from book:

    type string is array (positive range <>) of character;

    --

    constant LCD_display_len : positive := 20;
    subtype LCD_display_string is string(1 to LCD_display_len);
    variable LCD_display : LCD_display_string := (others => ' ');

    --

    type bit_vector is array (natural range <>) of bit;

    --
    
    subtype byte is bit_vector(7 downto 0);

    --

    variable channel_busy_register : bit_vector(1 to 4);

    --

    constant ready_message  : string := "Ready     ";

    --

    variable current_test : std_ulogic_vector(0 to 13) := "ZZZZZZZZZZ----";

    --

    constant all_ones : std_ulogic_vector(15 downto 0) := X"FFFF";

    -- end of code from book

  begin

    -- code from book:

    channel_busy_register := b"0000";

    -- end of code from book

    wait;
  end process process_2_b;


end architecture test;
