
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
-- $Id: ch_21_fg_21_05.vhd,v 1.2 2001-10-26 16:29:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity SR_flipflop is
  port ( s_n, r_n : in bit;  q, q_n : inout bit );

begin

  postponed process (q, q_n) is
            begin
              assert now = 0 fs or q = not q_n
                report "implementation error: q /= not q_n";
            end postponed process;

                          end entity SR_flipflop;

--------------------------------------------------

                                 architecture dataflow of SR_flipflop is
                                 begin

                                   gate_1 : q <= s_n nand q_n;
                                   gate_2 : q_n <= r_n nand q;

                                 end architecture dataflow;



-- not in book

                                 entity fg_21_05 is
                                 end entity fg_21_05;


                                 architecture test of fg_21_05 is

                                   signal s_n, r_n, q, q_n : bit;

                                 begin

                                   dut : entity work.SR_flipflop
                                     port map ( s_n, r_n, q, q_n );

                                   s_n <= '1',
                                          '0' after 10 ns, '1' after 15 ns,
                                          '0' after 30 ns, '1' after 40 ns;

                                   r_n <= '0', '1' after 5 ns,
                                          '0' after 20 ns, '1' after 25 ns,
                                          '0' after 30 ns, '1' after 35 ns;

                                 end architecture test;

-- end not in book
