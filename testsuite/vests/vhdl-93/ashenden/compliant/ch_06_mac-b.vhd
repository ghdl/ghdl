
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
-- $Id: ch_06_mac-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of mac is

  constant Tpd_clk_out : time := 3 ns;

  signal fp_x_real, fp_x_imag,
    fp_y_real, fp_y_imag,
    fp_s_real, fp_s_imag : real := 0.0;

begin

  x_real_converter : entity work.to_fp(behavioral)
    port map ( x_real, fp_x_real );

  x_imag_converter : entity work.to_fp(behavioral)
    port map ( x_imag, fp_x_imag );

  y_real_converter : entity work.to_fp(behavioral)
    port map ( y_real, fp_y_real );

  y_imag_converter : entity work.to_fp(behavioral)
    port map ( y_imag, fp_y_imag );

  behavior : process (clk) is

                             variable input_x_real, input_x_imag, input_y_real, input_y_imag : real := 0.0;
                           variable real_part_product_1, real_part_product_2,
                             imag_part_product_1, imag_part_product_2 : real := 0.0;
                           variable real_product, imag_product : real := 0.0;
                           variable real_sum, imag_sum : real := 0.0;
                           variable real_accumulator_ovf, imag_accumulator_ovf : boolean := false;

                           type boolean_to_stdulogic_table is array (boolean) of std_ulogic;
                           constant boolean_to_stdulogic : boolean_to_stdulogic_table
                             := (false => '0', true => '1');

  begin
    if rising_edge(clk) then
      -- work from the end of the pipeline back to the start, so as
      -- not to overwrite previous results in pipeline registers before
      -- they are used

      -- update accumulator and generate outputs
      if To_X01(clr) = '1' then
        real_sum := 0.0;
        real_accumulator_ovf := false;
        imag_sum := 0.0;
        imag_accumulator_ovf := false;
      else
        real_sum := real_product + real_sum;
        real_accumulator_ovf := real_accumulator_ovf
                                or real_sum < -16.0 or real_sum >= +16.0;
        imag_sum := imag_product + imag_sum;
        imag_accumulator_ovf := imag_accumulator_ovf 
                                or imag_sum < -16.0 or imag_sum >= +16.0;
      end if;
      fp_s_real <= real_sum after Tpd_clk_out;
      fp_s_imag <= imag_sum after Tpd_clk_out;
      ovf <= boolean_to_stdulogic(
        real_accumulator_ovf or imag_accumulator_ovf
        or real_sum < -1.0 or real_sum >= +1.0
        or imag_sum < -1.0 or imag_sum >= +1.0 )
             after Tpd_clk_out;

      -- update product registers using partial products
      real_product := real_part_product_1 - real_part_product_2;
      imag_product := imag_part_product_1 + imag_part_product_2;

      -- update partial product registers using latched inputs
      real_part_product_1 := input_x_real * input_y_real;
      real_part_product_2 := input_x_imag * input_y_imag;
      imag_part_product_1 := input_x_real * input_y_imag;
      imag_part_product_2 := input_x_imag * input_y_real;

      -- update input registers using MAC inputs
      input_x_real := fp_x_real;
      input_x_imag := fp_x_imag;
      input_y_real := fp_y_real;
      input_y_imag := fp_y_imag;
    end if;
  end process behavior;

  s_real_converter : entity work.to_vector(behavioral)
    port map ( fp_s_real, s_real );

  s_imag_converter : entity work.to_vector(behavioral)
    port map ( fp_s_imag, s_imag );

end architecture behavioral;
