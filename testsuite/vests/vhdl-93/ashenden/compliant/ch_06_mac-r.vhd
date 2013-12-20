
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
-- $Id: ch_06_mac-r.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture rtl of mac is

  signal pipelined_x_real,
    pipelined_x_imag,
    pipelined_y_real, 
    pipelined_y_imag : std_ulogic_vector(15 downto 0);
  signal real_part_product_1, 
    real_part_product_2,
    imag_part_product_1, 
    imag_part_product_2 : std_ulogic_vector(31 downto 0);
  signal pipelined_real_part_product_1,
    pipelined_real_part_product_2,
    pipelined_imag_part_product_1,
    pipelined_imag_part_product_2 : std_ulogic_vector(31 downto 0);
  signal real_product, 
    imag_product : std_ulogic_vector(32 downto 0);
  signal pipelined_real_product,
    pipelined_imag_product : std_ulogic_vector(19 downto 0);
  signal real_sum,
    imag_sum : std_ulogic_vector(21 downto 0);
  signal real_accumulator_ovf,
    imag_accumulator_ovf : std_ulogic;
  signal pipelined_real_sum,
    pipelined_imag_sum : std_ulogic_vector(21 downto 0);
  signal pipelined_real_accumulator_ovf,
    pipelined_imag_accumulator_ovf : std_ulogic;

begin

  x_real_input_reg : entity work.reg(behavioral)
    port map ( clk => clk, d => x_real, q => pipelined_x_real );

  x_imag_input_reg : entity work.reg(behavioral)
    port map ( clk => clk, d => x_imag, q => pipelined_x_imag );

  y_real_input_reg : entity work.reg(behavioral)
    port map ( clk => clk, d => y_real, q => pipelined_y_real );

  y_imag_input_reg : entity work.reg(behavioral)
    port map ( clk => clk, d => y_imag, q => pipelined_y_imag );

  real_mult_1 : entity work.multiplier(behavioral)
    port map ( a => pipelined_x_real, b => pipelined_y_real,
	       p => real_part_product_1 );

  real_mult_2 : entity work.multiplier(behavioral)
    port map ( a => pipelined_x_imag, b => pipelined_y_imag,
	       p => real_part_product_2 );

  imag_mult_1 : entity work.multiplier(behavioral)
    port map ( a => pipelined_x_real, b => pipelined_y_imag,
	       p => imag_part_product_1 );

  imag_mult_2 : entity work.multiplier(behavioral)
    port map ( a => pipelined_x_imag, b => pipelined_y_real,
	       p => imag_part_product_2 );

  real_part_product_reg_1 : entity work.reg(behavioral)
    port map ( clk => clk, d => real_part_product_1,
	       q => pipelined_real_part_product_1 );

  real_part_product_reg_2 : entity work.reg(behavioral)
    port map ( clk => clk, d => real_part_product_2,
	       q => pipelined_real_part_product_2 );

  imag_part_product_reg_1 : entity work.reg(behavioral)
    port map ( clk => clk, d => imag_part_product_1,
	       q => pipelined_imag_part_product_1 );

  imag_part_product_reg_2 : entity work.reg(behavioral)
    port map ( clk => clk, d => imag_part_product_2,
	       q => pipelined_imag_part_product_2 );

  real_product_subtracter : entity work.product_adder_subtracter(behavioral)
    port map ( mode => '1',
               a => pipelined_real_part_product_1,
	       b => pipelined_real_part_product_2,
	       s => real_product );

  imag_product_adder : entity work.product_adder_subtracter(behavioral)
    port map ( mode => '0',
               a => pipelined_imag_part_product_1,
	       b => pipelined_imag_part_product_2,
	       s => imag_product );

  real_product_reg : entity work.reg(behavioral)
    port map ( clk => clk,
	       d => real_product(32 downto 13),
	       q => pipelined_real_product );

  imag_product_reg : entity work.reg(behavioral)
    port map ( clk => clk,
	       d => imag_product(32 downto 13),
	       q => pipelined_imag_product );

  real_accumulator : entity work.accumulator_adder(behavioral)
    port map ( a(19 downto 0) => pipelined_real_product(19 downto 0),
	       a(20) => pipelined_real_product(19),
	       a(21) => pipelined_real_product(19),
	       b => pipelined_real_sum,
	       s => real_sum,
	       ovf => real_accumulator_ovf );

  imag_accumulator : entity work.accumulator_adder(behavioral)
    port map ( a(19 downto 0) => pipelined_imag_product(19 downto 0),
	       a(20) => pipelined_imag_product(19),
	       a(21) => pipelined_imag_product(19),
	       b => pipelined_imag_sum,
	       s => imag_sum,
	       ovf => imag_accumulator_ovf );

  real_accumulator_reg : entity work.accumulator_reg(behavioral)
    port map ( clk => clk, clr => clr,
	       d => real_sum,  q => pipelined_real_sum );

  imag_accumulator_reg : entity work.accumulator_reg(behavioral)
    port map ( clk => clk, clr => clr,
	       d => imag_sum,  q => pipelined_imag_sum );

  real_accumulator_ovf_reg : entity work.synch_sr_ff(behavioral)
    port map ( clk => clk,
               set => real_accumulator_ovf, clr => clr,
	       q => pipelined_real_accumulator_ovf );

  imag_accumulator_ovf_reg : entity work.synch_sr_ff(behavioral)
    port map ( clk => clk,
               set => imag_accumulator_ovf, clr => clr,
	       q => pipelined_imag_accumulator_ovf );

  s_real <= pipelined_real_sum(21) & pipelined_real_sum(16 downto 2);

  s_imag <= pipelined_imag_sum(21) & pipelined_imag_sum(16 downto 2);

  result_overflow_logic : entity work.overflow_logic(behavioral)
    port map ( real_accumulator_ovf => pipelined_real_accumulator_ovf,
	       imag_accumulator_ovf => pipelined_imag_accumulator_ovf,
	       real_sum => pipelined_real_sum(21 downto 17),
	       imag_sum => pipelined_imag_sum(21 downto 17),
	       ovf => ovf );

end architecture rtl;
