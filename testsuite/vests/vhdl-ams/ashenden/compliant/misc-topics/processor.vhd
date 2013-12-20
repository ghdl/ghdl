
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

entity processor is
end entity processor;


-- code from book

architecture rtl of processor is

  component latch is
    generic ( width : positive );
    port ( d : in std_ulogic_vector(0 to width - 1);
           q : out std_ulogic_vector(0 to width - 1);
           -- . . . );
           -- not in book
           other_port : in std_ulogic := '-' );
           -- end not in book
  end component latch;

  component ROM is
    port ( d_out : out std_ulogic_vector;  -- . . . );
           -- not in book
           other_port : in std_ulogic := '-' );
           -- end not in book
  end component ROM;

  subtype std_logic_word is std_logic_vector(0 to 31);

  signal source1, source2, destination : std_logic_word;
  -- . . .

begin

  temp_register : component latch
    generic map ( width => 32 )
    port map ( d => std_ulogic_vector(destination),
               std_logic_vector(q) => source1, -- . . . );
               -- not in book
               other_port => open );
               -- end not in book

  constant_ROM : component ROM
    port map ( std_logic_word(d_out) => source2, -- . . . );
               -- not in book
               other_port => open );
               -- end not in book

  -- . . .

end architecture rtl;

-- end code from book
