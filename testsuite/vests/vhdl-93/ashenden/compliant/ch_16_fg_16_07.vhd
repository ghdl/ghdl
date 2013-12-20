
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
-- $Id: ch_16_fg_16_07.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity fg_16_07 is
               end entity fg_16_07;


               architecture test of fg_16_07 is

                 constant reg0 : std_logic_vector(7 downto 0) := "00000000";
                 constant reg1 : std_logic_vector(7 downto 0) := "11111111";
                 signal dbus : std_logic_vector(7 downto 0);
                 signal reg_sel, read, reg_addr : X01 := '0';

               begin

                 -- code from book

                 reg_read_selector : block (reg_sel = '1' and read = '1') is
                 begin
                   dbus <= reg0 when guard and reg_addr = '0' else
                           reg1 when guard and reg_addr = '1' else
                           "ZZZZZZZZ";
                 end block reg_read_selector;

                 -- end code from book

                 stimulus : process is
                 begin
                   reg_sel <= '1';  wait for 10 ns;
                   read <= '1', '0' after 5 ns;  wait for 10 ns;
                   reg_sel <= '0';  wait for 10 ns;
                   read <= '1', '0' after 5 ns;  wait for 10 ns;
                   reg_addr <= '1';  wait for 10 ns;
                   reg_sel <= '1';  wait for 10 ns;
                   read <= '1', '0' after 5 ns;  wait for 10 ns;

                   wait;
                 end process stimulus;

               end architecture test;
