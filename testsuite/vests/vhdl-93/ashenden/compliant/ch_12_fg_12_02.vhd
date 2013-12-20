
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
-- $Id: ch_12_fg_12_02.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- code from book

entity D_flipflop is
  generic ( Tpd_clk_q, Tsu_d_clk, Th_d_clk : delay_length );
  port ( clk, d : in bit;  q : out bit );
end entity D_flipflop;

--------------------------------------------------

architecture basic of D_flipflop is
begin

  behavior : q <= d after Tpd_clk_q when clk = '1' and clk'event;

  check_setup : process is
  begin
    wait until clk = '1';
    assert d'last_event >= Tsu_d_clk
      report "setup violation";
  end process check_setup;

  check_hold : process is
  begin
    wait until clk'delayed(Th_d_clk) = '1';
    assert d'delayed'last_event >= Th_d_clk
      report "hold violation";
  end process check_hold;

end architecture basic;

-- end code from book



entity fg_12_02 is
end entity fg_12_02;



architecture test of fg_12_02 is

  signal system_clock, request, request_pending : bit := '0';

begin

  -- code from book (in text)

  request_flipflop : entity work.D_flipflop(basic)
    generic map ( Tpd_clk_q => 4 ns,
                  Tsu_d_clk => 3 ns, Th_d_clk => 1 ns )
    port map ( clk => system_clock,
               d => request, q => request_pending );

  -- end code from book

  clock_gen : system_clock <= '1' after 10 ns,
                              '0' after 20 ns when system_clock = '0';

  stimulus :  request <= '1' after  25 ns, '0' after  35 ns,
                         '1' after  67 ns, '0' after  71 ns,
                         '1' after 108 ns, '0' after 110.5 ns;
  
end architecture test;
