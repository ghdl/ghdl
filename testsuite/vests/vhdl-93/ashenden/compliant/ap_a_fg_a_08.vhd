
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
-- $Id: ap_a_fg_a_08.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

library ieee;  use ieee.std_logic_1164.all;

               entity entname is
               end entity entname;

-- end not in book


               architecture rtl of entname is

                 type state_type is (s0, s1, s2, s3);
                 signal state, next_state : state_type;
                 signal con1, con2, con3 : std_ulogic;
                 signal out1, out2 : std_ulogic;
                 signal clk, reset : std_ulogic;
                 -- . . .

               begin
                 state_logic : process (state, con1, con2, con3) is
                 begin
                   case state is
                     when s0 =>
                       out1 <= '0';
                       out2 <= '0';
                       next_state <= s1; 
                     when s1 =>
                       out1 <= '1';
                       if con1 = '1' then
                         next_state <= s2;
                       else
                         next_state <= s1;
                       end if;
                     when s2 =>
                       out2 <= '1';
                       next_state <= s3;
                     when s3 =>
                       if con2 = '0' then
                         next_state <= s3;
                       elsif con3 = '0' then
                         out1 <= '0';
                         next_state <= s2;
                       else
                         next_state <= s1;
                       end if;
                   end case;
                 end process state_logic;

                 state_register : process (clk, reset) is
                 begin
                   if reset = '0' then
                     state <= s0;
                   elsif rising_edge(clk) then
                     state <= next_state;
                   end if;
                 end process state_register;

                 -- . . .

                 -- not in book

                 clk_gen : process is
                 begin
                   clk <= '0', '1' after 10 ns;
                   wait for 20 ns;
                 end process clk_gen;

                 reset <= '0', '1' after 40 ns;

                 con1 <= '0', '1' after 100 ns, '0' after 120 ns;

                 con2 <= '0', '1' after 160 ns;

                 con3 <= '0', '1' after 220 ns;

                 -- end not in book

               end architecture rtl;
