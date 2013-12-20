
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
-- $Id: ch_16_fg_16_13.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture detailed_timing of counter is

  signal reset_ipd,                   -- data input port delayed
    clk_ipd : bit;               -- clock input port delayed
  signal q_zd : bit_vector(q'range);  -- q output with zero delay

begin

  input_port_delay : block is
  begin
    reset_ipd <= reset after tipd_reset;
    clk_ipd <= clk after tipd_clk;
  end block input_port_delay;

  functionality : block is

                          function increment ( bv : bit_vector ) return bit_vector is
    variable result : bit_vector(bv'range) := bv;
    variable carry : bit := '1';
  begin
    for index in result'reverse_range loop
      result(index) := bv(index) xor carry;
      carry :=  bv(index) and carry;
      exit when carry = '0';
    end loop;
    return result;
  end function increment;

  signal next_count : bit_vector(q'range);

  begin
    next_count <= increment(q_zd) when reset_ipd = '0' else
                  (others => '0');
    q_zd <= next_count when clk_ipd = '1' and clk_ipd'event;
  end block functionality;

  output_port_delay : block is
  begin
    q <= q_zd after topd_q;
  end block output_port_delay;

  timing_checks : block is
  begin
    -- check setup time: reset before clk
    -- . . .
    -- check hold time: reset after clk
    -- . . .
  end block timing_checks;

end architecture detailed_timing;


-- not in book

entity fg_16_13 is
end entity fg_16_13;


architecture test of fg_16_13 is

  signal reset, clk : bit := '0';
  signal q : bit_vector(3 downto 0);

begin

  dut : entity work.counter(detailed_timing)
    generic map ( tipd_reset => 2 ns,
                  tipd_clk => 3 ns,
                  topd_q => 4 ns,
                  tsetup_reset => 3 ns,
                  thold_reset => 1 ns )
    port map ( reset => reset, clk => clk, q => q );

  clk_gen : clk <= '1' after 10 ns, '0' after 20 ns when clk = '0';

  reset <= '1' after 62 ns, '0' after 106 ns;

end architecture test;

-- end not in book
