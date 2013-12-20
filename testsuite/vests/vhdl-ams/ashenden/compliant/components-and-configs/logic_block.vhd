
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

-- code from book (in text)

entity nand3 is
  port ( a, b, c : in bit;  y : out bit );
end entity nand3;

-- end code from book

architecture behavioral of nand3 is
begin
  y <= not (a and b and c);
end architecture behavioral;



entity logic_block is
end entity logic_block;


-- code from book

library gate_lib;

architecture ideal of logic_block is

  component nand2 is
    port ( in1, in2 : in bit;  result : out bit );
  end component nand2;

  for all : nand2
    use entity gate_lib.nand3(behavioral)
    port map ( a => in1, b => in2, c => '1', y => result );

  -- . . .    -- other declarations

  -- not in book
  signal s1, s2, s3 : bit := '0';

begin

  gate1 : component nand2
    port map ( in1 => s1, in2 => s2, result => s3 );

  -- . . .    -- other concurrent statements

  -- not in book

  s1 <= '1' after 20 ns;

  s2 <= '1' after 10 ns, '0' after 20 ns, '1' after 30 ns;

  -- end not in book

end architecture ideal;

-- end code from book
