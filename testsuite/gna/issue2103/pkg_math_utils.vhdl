------------------------------------------------------------------------------
--          ____  _____________  __                                         --
--         / __ \/ ____/ ___/\ \/ /                 _   _   _               --
--        / / / / __/  \__ \  \  /                 / \ / \ / \              --
--       / /_/ / /___ ___/ /  / /               = ( M | S | K )=            --
--      /_____/_____//____/  /_/                   \_/ \_/ \_/              --
--                                                                          --
------------------------------------------------------------------------------
--! @copyright Copyright 2022 DESY
--! SPDX-License-Identifier: CERN-OHL-W-2.0
------------------------------------------------------------------------------
--! @date 2022-04-01
--! @author Michael Buechler <michael.buechler@desy.de>
--! @author Lukasz Butkowski <lukasz.butkowski@desy.de>
------------------------------------------------------------------------------
--! @brief
--! Math utilities
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

--! Package of mathematical utilities and support functions
package math_utils is

  --! Saturation status after an operation.
  type t_saturation is (ST_SAT_OK, ST_SAT_OVERFLOWN, ST_SAT_UNDERFLOWN);

  --! The encoding for _t_staturation_ is
  --!
  --! * ST_SAT_OK         = "00" (No saturation)
  --! * ST_SAT_OVERFLOWN  = "10" (Operation has overflown)
  --! * ST_SAT_UNDERFLOWN = "01" (Operation has underflown)
  attribute enum_encoding : string;
  attribute enum_encoding of t_saturation : type is "00 10 01";

  --! f_bit_length:
  --! Comparable to Python's int.bit_length(),
  --! but when arg is negative, calculate for two's complement.
  --! Attention: returns 1 for argument -1.
  --! When arg is zero, return 0.
  function f_bit_length (arg: integer) return integer;

  --! f_unsigned_length:
  --! Calculates the minimum unsigned signal length to store **arg**,
  --! When arg is zero, return 0.
  function f_unsigned_length (arg: natural) return natural;

  --! Calculates the minimum signed signal length to store *arg*,
  --! When arg is zero, return 0.
  function f_signed_length (arg: integer) return integer;

  --! f_maximum:
  --! Porting of 'maximum' from VHDL 08' not present in the
  --! 93' standard.
  function f_maximum (a, b: integer) return integer;

  --! f_minimum:
  --! Porting of 'minimum' from VHDL 08' not present in the
  --! 93' standard
  function f_minimum (a, b: integer) return integer;

  --! f_max_val_for_length:
  --! Returns the maximum value representable by a numeric type of length
  --! _length_ and sign _sign_.
  function f_max_val_for_length (length: natural; sign : boolean) return integer;

  function f_max_val_of (arg: unsigned) return unsigned;

  --! f_max_val_of:
  --! Returns the maximum value representable by a numeric type.
  --! Signed and unsigned version.
  function f_max_val_of (arg: signed) return signed;

  --! f_max_val_for_length:
  --! Returns the maximum value representable by a numeric type of length
  --! _length_ and sign _sign_.
  function f_min_val_for_length (length: natural; sign : boolean) return integer;

  function f_min_val_of (arg: unsigned) return unsigned;

  --! f_min_val_of:
  --! Returns the minimum value representable by a numeric type.
  --! Signed and unsigned version.
  function f_min_val_of (arg: signed) return signed;

  function f_is_max (arg: unsigned) return boolean;

  --! f_is_max:
  --! Checks whether a signal is at its maximum value.
  --! Signed and unsigned version.
  function f_is_max (arg: signed) return boolean;

  function f_is_min (arg: unsigned) return boolean;

  --! f_is_min:
  --! Checks whether a signal is at its minimum value.
  --! Signed and unsigned version.
  function f_is_min (arg: signed) return boolean;

end package math_utils;

--******************************************************************************

--******************************************************************************

package body math_utils is

  function f_bit_length (arg: integer) return integer is
  begin

    if (arg = 0) then
      return 0;
    elsif (arg > 0) then
      return integer(ceil(log2(real(arg + 1))));
    else
      return integer(ceil(log2(-real(arg)))) + 1;
    end if;

  end function;

  function f_unsigned_length (arg: natural) return natural is
  begin

    return natural(f_bit_length(integer(arg)));

  end function;

  function f_signed_length (arg: integer) return integer is
  begin

    if (arg >= 0) then
      return f_bit_length(arg) + 1;
    else
      return f_bit_length(arg);
    end if;

  end function;

  function f_maximum (a, b: integer) return integer is
  begin

    if (a > b) then
      return a;
    else
      return b;
    end if;

  end function;

  function f_minimum (a, b: integer) return integer is
  begin

    if (a < b) then
      return a;
    else
      return b;
    end if;

  end function;

  function f_max_val_for_length (length: natural; sign : boolean) return integer is

    constant C_SMAX : integer := (2 ** (length - 1)) - 1;
    constant C_UMAX : integer := (2 ** length) - 1;

  begin

    if (sign) then
      return C_SMAX;
    else
      return C_UMAX;
    end if;

  end function;

  function f_max_val_of (arg: unsigned) return unsigned is
  begin

    return to_unsigned(f_max_val_for_length(arg'length, false), arg'length);

  end function;

  function f_max_val_of (arg: signed) return signed is
  begin

    return to_signed(f_max_val_for_length(arg'length, true), arg'length);

  end function;

  function f_min_val_for_length (length: natural; sign : boolean) return integer is

    constant C_SMIN : integer := - (2 ** (length - 1));
    constant C_UMIN : integer := 0;

  begin

    if (sign) then
      return C_SMIN;
    else
      return C_UMIN;
    end if;

  end function;

  function f_min_val_of (arg: unsigned) return unsigned is
  begin

    return to_unsigned(f_min_val_for_length(arg'length, false), arg'length);

  end function;

  function f_min_val_of (arg: signed) return signed is
  begin

    return to_signed(f_min_val_for_length(arg'length, true), arg'length);

  end function;

  function f_is_max (arg: unsigned) return boolean is
  begin

    return arg = f_max_val_of(arg);

  end;

  function f_is_max (arg: signed) return boolean is
  begin

    return arg = f_max_val_of(arg);

  end;

  function f_is_min (arg: unsigned) return boolean is
  begin

    return arg = f_min_val_of(arg);

  end;

  function f_is_min (arg: signed) return boolean is
  begin

    return arg = f_min_val_of(arg);

  end;

end package body math_utils;
