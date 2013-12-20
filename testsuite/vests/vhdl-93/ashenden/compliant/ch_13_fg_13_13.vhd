
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
-- $Id: ch_13_fg_13_13.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

-- not in book

entity computer_system is
end entity computer_system;


library stimulus;  
use stimulus.stimulus_generators.all;

-- end not in book

architecture structure of computer_system is

  component decoder_2_to_4 is
                             generic ( prop_delay : delay_length );
                           port ( in0, in1 : in bit;
                                  out0, out1, out2, out3 : out bit );
  end component decoder_2_to_4;

  -- . . .

  -- not in book

  signal addr : bit_vector(5 downto 4);
  signal interface_a_select, interface_b_select,
    interface_c_select, interface_d_select : bit;
  -- end not in book

begin

  interface_decoder : component decoder_2_to_4
    generic map ( prop_delay => 4 ns )
    port map ( in0 => addr(4), in1 => addr(5),
               out0 => interface_a_select, out1 => interface_b_select,
               out2 => interface_c_select, out3 => interface_d_select );

  -- . . .

  -- not in book

  all_possible_values(addr, 10 ns);

  -- end not in book

end architecture structure;
