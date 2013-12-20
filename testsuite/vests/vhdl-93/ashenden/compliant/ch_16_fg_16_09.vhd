
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
-- $Id: ch_16_fg_16_09.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity latch is
  generic ( width : positive );
  port ( enable : in bit;
         d : in bit_vector(0 to width - 1);
         q : out bit_vector(0 to width - 1) );
end entity latch;

--------------------------------------------------

architecture behavioral of latch is
begin

  transfer_control : block ( enable = '1' ) is
  begin
    q <= guarded d;
  end block transfer_control;

end architecture behavioral;


-- not in book

entity fg_16_09 is
end entity fg_16_09;


architecture test of fg_16_09 is

  signal enable : bit := '0';
  signal d, q : bit_vector(0 to 7);

begin

  dut : entity work.latch(behavioral)
    generic map ( width => 8 )
    port map ( enable => enable, d => d, q => q );

  stimulus : process is
  begin
    wait for 10 ns;
    d <= X"11";  wait for 10 ns;
    enable <= '1';  wait for 10 ns;
    d <= X"AA";  wait for 10 ns;
    enable <= '0';  wait for 10 ns;
    d <= X"00";  wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;

-- end not in book
