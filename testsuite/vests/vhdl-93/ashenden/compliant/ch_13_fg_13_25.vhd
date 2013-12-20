
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
-- $Id: ch_13_fg_13_25.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity nand3 is
  generic ( Tpd : delay_length );
  port ( a, b, c : in bit;  y : out bit );
end entity nand3;

architecture basic of nand3 is
begin
  y <= not (a and b and c) after Tpd;
end architecture basic;


library project_lib;
library stimulus;  
use stimulus.stimulus_generators.all;

entity misc_logic is
end entity misc_logic;

-- code from book

architecture gate_level of misc_logic is

  component nand3 is
                    generic ( Tpd : delay_length );
                  port ( a, b, c : in bit;  y : out bit );
  end component nand3;

  for all : nand3
    use entity project_lib.nand3(basic);

  -- . . .

  -- not in book
  signal sig1, sig2, sig3, out_sig : bit;
  signal test_vector : bit_vector(1 to 3);
  -- end not in book

begin

  gate1 : component nand3
    generic map ( Tpd => 2 ns )
    port map ( a => sig1, b => sig2, c => sig3, y => out_sig );

  -- . . .

  -- not in book

  all_possible_values(test_vector, 10 ns);

  (sig1, sig2, sig3) <= test_vector;

  -- end not in book

end architecture gate_level;

-- end code from book
