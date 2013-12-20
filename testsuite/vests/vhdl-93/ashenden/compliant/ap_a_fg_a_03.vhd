
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
-- $Id: ap_a_fg_a_03.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- code from book

library ieee;  use ieee.std_logic_1164.all;

               entity add_and_sub is
                 port ( a, b, c : in natural;
                        y : out natural;
                        ovf : out std_ulogic );
               end entity add_and_sub;

--------------------------------------------------

               library ieee;  use ieee.numeric_std.all;

               architecture rtl of add_and_sub is
                 signal stage2, stage3 : unsigned ( 8 downto 0 );
               begin
                 stage2 <= To_unsigned(a, 9) + to_unsigned(b, 9);  -- "+" from numeric_std
                 stage3 <= stage2 - c;              -- "-" from numeric_std
                 y <= To_integer(stage3) ;
                 ovf <= stage3(8);
               end rtl;

-- end code from book



               entity fg_a_03 is
               end entity fg_a_03;


               library ieee;
               use ieee.std_logic_1164.all, ieee.numeric_std.all;

               architecture test of fg_a_03 is

                 signal a, b, c, y : natural := 0;
                 signal ovf : std_ulogic;

               begin

                 dut : entity work.add_and_sub
                   port map ( a, b, c, y, ovf );

                 stimulus : process is
                 begin
                   wait for 10 ns;
                   a <= 2;  b <= 5;  c <= 3;  wait for 10 ns;
                   a <= 192;  b <= 192;  wait for 10 ns;
                   a <= 10;  b <= 11;  c <= 22;  wait for 10 ns;

                   wait;
                 end process stimulus;

               end architecture test;
