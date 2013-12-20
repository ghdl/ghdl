
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
-- $Id: ch_06_mact-bv.vhd,v 1.3 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

architecture bench_verify of mac_test is

  signal clk, clr, behavioral_ovf, rtl_ovf : std_ulogic := '0';
  signal x_real, x_imag,
    y_real, y_imag,
    behavioral_s_real, behavioral_s_imag,
    rtl_s_real, rtl_s_imag : std_ulogic_vector(15 downto 0);

  type complex is record
                    re, im : real;
                  end record;

  signal x, y, behavioral_s, rtl_s : complex := (0.0, 0.0);

  constant Tpw_clk : time := 50 ns;

begin

  x_real_converter : entity work.to_vector(behavioral) port map (x.re, x_real);
  x_imag_converter : entity work.to_vector(behavioral) port map (x.im, x_imag);
  y_real_converter : entity work.to_vector(behavioral) port map (y.re, y_real);
  y_imag_converter : entity work.to_vector(behavioral) port map (y.im, y_imag);

  dut_behavioral : entity work.mac(behavioral)
    port map ( clk, clr,
               x_real, x_imag, y_real, y_imag,
               behavioral_s_real, behavioral_s_imag, behavioral_ovf );

  dut_rtl : entity work.mac(rtl)
    port map ( clk, clr,
               x_real, x_imag, y_real, y_imag,
               rtl_s_real, rtl_s_imag, rtl_ovf );

  behavioral_s_real_converter :
    entity work.to_fp(behavioral) port map (behavioral_s_real, behavioral_s.re);
  behavioral_s_imag_converter :
    entity work.to_fp(behavioral) port map (behavioral_s_imag, behavioral_s.im);

  rtl_s_real_converter :
    entity work.to_fp(behavioral) port map (rtl_s_real, rtl_s.re);
  rtl_s_imag_converter :
    entity work.to_fp(behavioral) port map (rtl_s_imag, rtl_s.im);


  clock_gen : process is
  begin
    clk <= '1' after Tpw_clk, '0' after 2 * Tpw_clk;
    wait for 2 * Tpw_clk;
  end process clock_gen;


  stimulus : process is
  begin
    -- first sequence
    clr <= '1';  wait until clk = '0';
    x <= (+0.5, +0.5);	y <= (+0.5, +0.5);  clr <= '1';	 wait until clk = '0';
    x <= (+0.2, +0.2);	y <= (+0.2, +0.2);  clr <= '1';	 wait until clk = '0';
    x <= (+0.1, -0.1);	y <= (+0.1, +0.1);  clr <= '1';	 wait until clk = '0';
    x <= (+0.1, -0.1);	y <= (+0.1, +0.1);  clr <= '0';	 wait until clk = '0';

    -- should be (0.4, 0.58) when it falls out the other end

    clr <= '0';	 wait until clk = '0';
    x <= (+0.5, +0.5);	y <= (+0.5, +0.5);  clr <= '0';	 wait until clk = '0';
    x <= (+0.5, +0.5);	y <= (+0.1, +0.1);  clr <= '0';	 wait until clk = '0';
    x <= (+0.5, +0.5);	y <= (+0.5, +0.5);  clr <= '1';	 wait until clk = '0';
    x <= (-0.5, +0.5);	y <= (-0.5, +0.5);  clr <= '0';	 wait until clk = '0';
    clr <= '0';	 wait until clk = '0';
    clr <= '0';	 wait until clk = '0';
    clr <= '0';	 wait until clk = '0';
    clr <= '1';	 wait until clk = '0';

    wait;
  end process stimulus;


  verifier : process

    constant epsilon : real := 4.0E-5;  -- 1-bit error in 15-bit mantissa

  begin
    wait until clk = '0';
    assert behavioral_ovf = rtl_ovf
      report "Overflow flags differ" severity error;
    if behavioral_ovf = '0' and rtl_ovf = '0' then
      assert abs (behavioral_s.re - rtl_s.re) < epsilon
        report "Real sums differ" severity error;
      assert abs (behavioral_s.im - rtl_s.im) < epsilon
        report "Imag sums differ" severity error;
    end if;
  end process verifier;

end architecture bench_verify;
