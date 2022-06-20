--------------------------------------------------------------------------------          ____  _____________  __                                         ----         / __ \/ ____/ ___/\ \/ /                 _   _   _               ----        / / / / __/  \__ \  \  /                 / \ / \ / \              ----       / /_/ / /___ ___/ /  / /               = ( M | S | K )=            ----      /_____/_____//____/  /_/                   \_/ \_/ \_/              ----                                                                          ----------------------------------------------------------------------------------! @copyright Copyright 2022 DESY--! SPDX-License-Identifier: CERN-OHL-W-2.0--------------------------------------------------------------------------------! @date 2022-06-07--! @author Andrea Bellandi--! @email  andrea.bellandi@desy.de--------------------------------------------------------------------------------! @brief--! Miscellaneous logic utilities------------------------------------------------------------------------------library ieee;use ieee.std_logic_1164.all;--!---------------------------------------------------------! Miscellaneous logic functions. All ported functions from--! IEEE.std_logic_misc belongs here.package logic_misc is  --!-------------------------------------------------------  --! f_to_std_logic:  --! Converts std_logic to boolean.  function f_to_std_logic (arg: boolean) return std_logic;  --!-------------------------------------------------------  --! f_to_boolean:  --! Converts std_logic to boolean  function f_to_boolean (arg: std_logic) return boolean;  --!-------------------------------------------------------  --! f_all_ones:  --! Checks whether all bits of **arg** are equal to '1'.  --! Equivalent to _and_reduce_.  function f_all_ones (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! f_all_zeroes:  --! Checks wheter all bits of **arg** are equal to '0'.  --! Equivalent to _nor_reduce_.  function f_all_zeroes (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! f_odd_ones:  --! Return '1' if an odd number of bits in **arg** are '1'.  --! Equivalent to _xor_reduce_.  function f_odd_ones (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! f_even_ones:  --! Return '1' if an even number of bits in **arg** are '1'.  --! Return '1' if zero bits in **arg** are '1'.  --! Equivalent to _xnor_reduce_.  function f_even_ones (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! f_odd_zeroes:  --! Return '1' if an odd number of bits in **arg** are '0'.  --! Equivalent to _xor_reduce_.  function f_odd_zeroes (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! f_even_zeroes:  --! Return '1' if an even number of bits in **arg** are '0'.  --! Return '1' if zero bits in **arg** are '0'.  --! Equivalent to _xnor_reduce_.  function f_even_zeroes (arg: std_logic_vector) return std_logic;  -- vsg_off function_101  --!-------------------------------------------------------  --! or_reduce:  --! Reduction of bits in **arg** with the **or** logical operator.  --! Port of nonstandard _ieee.std_logic_misc.or_reduce_  function or_reduce (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! and_reduce:  --! Reduction of bits in **arg** with the **and** logical operator.  --! Port of nonstandard _ieee.std_logic_misc.and_reduce_  function and_reduce (arg: std_logic_vector) return std_logic;  --!-------------------------------------------------------  --! xor_reduce:  --! Reduction of bits in **arg** with the **xor** logical operator.  --! Port of nonstandard _ieee.std_logic_misc.xor_reduce_  function xor_reduce (arg: std_logic_vector) return std_logic;  --!-------------------------
  --! nor_reduce:
  --! Negated reduction of bits in **arg** with the **or** logical operator.
  --! Port of nonstandard _ieee.std_logic_misc.nor_reduce_

  function nor_reduce (arg: std_logic_vector) return std_logic;

  --!-------------------------------------------------------
  --! nand_reduce:
  --! Negated reduction of bits in **arg** with the **and** logical operator.
  --! Port of nonstandard _ieee.std_logic_misc.nand_reduce_

  function nand_reduce (arg: std_logic_vector) return std_logic;

  --!-------------------------------------------------------
  --! xnor_reduce:
  --! Negated reduction of bits in **arg** with the **xor** logical operator.
  --! Port of nonstandard _ieee.std_logic_misc.xnor_reduce_

  function xnor_reduce (arg: std_logic_vector) return std_logic;

-- vsg_on

end package logic_misc;

package body logic_misc is

  function f_to_std_logic (arg: boolean) return std_logic is
  begin

    if (arg) then
      return '1';
    else
      return '0';
    end if;

  end function;

  function f_to_boolean (arg: std_logic) return boolean is
  begin

    return arg = '1';

  end function;

  function f_all_ones (arg: std_logic_vector) return std_logic is

    constant C_ONES : std_logic_vector(arg'length - 1 downto 0) :=
    (
      others => '1'
    );

  begin

    return f_to_std_logic(arg = C_ONES);

  end function;

  function f_all_zeroes (arg: std_logic_vector) return std_logic is

    constant C_ZEROES : std_logic_vector(arg'length - 1 downto 0) :=
    (
      others => '0'
    );

  begin

    return f_to_std_logic(arg = C_ZEROES);

  end function;

  function f_odd_ones (arg: std_logic_vector) return std_logic is

    variable var_result : std_logic := '0';

  begin

    for i in arg'low to arg'high loop

      var_result := var_result xor arg(i);

    end loop;

    return var_result;

  end function;

  function f_even_ones (arg: std_logic_vector) return std_logic is
  begin

    return not f_odd_ones(arg);

  end function;

  function f_odd_zeroes (arg: std_logic_vector) return std_logic is
  begin

    return f_odd_ones(not arg);

  end function;

  function f_even_zeroes (arg: std_logic_vector) return std_logic is
  begin

    return not f_odd_zeroes(arg);

  end function;

  -- vsg_off function_101

  function or_reduce (arg: std_logic_vector) return std_logic is
  begin

    return not f_all_zeroes(arg);

  end function;

  function and_reduce (arg: std_logic_vector) return std_logic is
  begin

    return f_all_ones(arg);

  end function;

  function xor_reduce (arg: std_logic_vector) return std_logic is
  begin

    return f_odd_ones(arg);

  end function;

  function nor_reduce (arg: std_logic_vector) return std_logic is
  begin

    return f_all_zeroes(arg);

  end function;

  function nand_reduce (arg: std_logic_vector) return std_logic is
  begin

    return not f_all_ones(arg);

  end function;

  function xnor_reduce (arg: std_logic_vector) return std_logic is
  begin

    return f_even_ones(arg);

  end function;

-- vsg_on

end package body logic_misc;
