
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
-- $Id: ch_16_fg_16_05.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

use work.fg_16_04.all;

-- code from book (in text)

entity tri_state_reg is
  port ( d : in resolved_byte;
         q : out resolved_byte bus;
         clock, out_enable : in bit );
end entity tri_state_reg;

-- end code from book



-- code from book

architecture behavioral of tri_state_reg is
begin

  reg_behavior : process (d, clock, out_enable) is
                                                  variable stored_byte : byte;
  begin
    if clock'event and clock = '1' then
      stored_byte := d;
    end if;
    if out_enable = '1' then
      q <= stored_byte;
    else
      q <= null;
    end if;
  end process reg_behavior;

end architecture behavioral;

-- end code from book



use work.fg_16_04.all;

entity fg_16_05 is
end entity fg_16_05;


architecture test of fg_16_05 is

  signal d1, d2, q : resolved_byte := X"00";
  signal clk1, clk2, oe1, oe2 : bit := '0';

begin

  dut1 : entity work.tri_state_reg(behavioral)
    port map ( d => d1, q => q, clock => clk1, out_enable => oe1 );

  dut2 : entity work.tri_state_reg(behavioral)
    port map ( d => d2, q => q, clock => clk2, out_enable => oe2 );

  stimulus : process is
  begin
    d1 <= X"11";  clk1 <= '1', '0' after 5 ns;  wait for 10 ns;
    oe1 <= '1', '0' after 5 ns;  wait for 10 ns;
    d2 <= X"21";  clk2 <= '1', '0' after 5 ns;  wait for 10 ns;
    oe2 <= '1', '0' after 5 ns;  wait for 10 ns;
    oe1 <= '1', '0' after 5 ns;
    oe2 <= '1', '0' after 5 ns;

    wait;
  end process stimulus;

end architecture test;
