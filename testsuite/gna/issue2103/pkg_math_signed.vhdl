
------------------------------------------------------------------------------
--          ____  _____________  __                                         --
--         / __ \/ ____/ ___/\ \/ /                 _   _   _               --
--        / / / / __/  \__ \  \  /                 / \ / \ / \              --
--       / /_/ / /___ ___/ /  / /               = ( M | S | K )=            --
--      /_____/_____//____/  /_/                   \_/ \_/ \_/              --
--                                                                          --
------------------------------------------------------------------------------
--! @copyright Copyright 2020-2022 DESY
--! SPDX-License-Identifier: CERN-OHL-W-2.0
------------------------------------------------------------------------------
--! @date 2020-10-02/2022-04-01
--! @author Lukasz Butkowski <lukasz.butkowski@desy.de>
--! @author Michael Buechler <michael.buechler@desy.de>
------------------------------------------------------------------------------
--! @brief
--! Provides math function/procedures with signed signals
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.math_utils.all;
use work.logic_misc.all;

package math_signed is

  --! Saturate *arg* to the maximum or minimum representable value depending on
  --! sat
  function f_saturate (arg : signed; sat : t_saturation) return signed;

  --! Resize with saturation protection
  function f_resize_sat (arg : signed; length : positive) return signed;

  --! Resize with saturation protection and saturation flag
  --! Like the <<f_resize_sat>> function but also sets an overflow bit.
  --! To be able to combine this with another preceding operation like shift_right
  --! in a process, the argument 'arg' must be a variable instead of a signal.
  procedure prd_resize_sat (
    signal arg      : in signed;
    constant length : in positive;
    signal result   : out signed;
    signal sat      : out t_saturation
  );

end package math_signed;

--******************************************************************************

package body math_signed is

  function f_saturate (arg : signed; sat : t_saturation) return signed is
  begin

    if (sat = ST_SAT_OVERFLOWN) then
      return f_max_val_of(arg);
    elsif (sat = ST_SAT_UNDERFLOWN) then
      return f_min_val_of(arg);
    else
      return arg;
    end if;

  end function f_saturate;

  function f_resize_sat (arg : signed; length : natural) return signed is

    variable var_result : signed(length - 1 downto 0);

  begin

    prd_resize_sat(arg => arg, length => length, result => var_result);
    return var_result;

  end function;

  procedure prd_resize_sat (
    signal arg      : in signed;
    constant length : in positive;
    signal result   : out signed;
    signal sat      : out t_saturation
  ) is

    variable var_sat : t_saturation;

  begin

    if (length >= arg'length) then
      var_sat := ST_SAT_OK;
    else
      -- check overflow saturation
      if (arg(arg'high) = '0' and
          f_all_zeroes(arg(arg'high-1 downto length - 1)) = '0') then
        var_sat := ST_SAT_OVERFLOWN;
      -- check underflow saturation
      elsif (arg(arg'high) = '1' and
             f_all_ones(arg(arg'high-1 downto length - 1)) = '0') then
        var_sat := ST_SAT_UNDERFLOWN;
      else
        var_sat := ST_SAT_OK;
      end if;
      result <= f_saturate(resize(arg, length), var_sat);
      sat    <= var_sat;
    end if;

  end procedure prd_resize_sat;

end package body math_signed;
