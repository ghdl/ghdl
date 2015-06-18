------------------------------------------------------------------------------
-- "standard_additions" package contains the additions to the built in
-- "standard.std" package.  In the final version this package will be implicit.
--  Created for VHDL-200X par, David Bishop (dbishop@vhdl.org)
------------------------------------------------------------------------------
package standard_additions is

  function \?=\ (L, R  : BOOLEAN) return BOOLEAN;
  function \?/=\ (L, R : BOOLEAN) return BOOLEAN;
  function \?<\ (L, R  : BOOLEAN) return BOOLEAN;
  function \?<=\ (L, R : BOOLEAN) return BOOLEAN;
  function \?>\ (L, R  : BOOLEAN) return BOOLEAN;
  function \?>=\ (L, R : BOOLEAN) return BOOLEAN;

  function MINIMUM (L, R : BOOLEAN) return BOOLEAN;
  function MAXIMUM (L, R : BOOLEAN) return BOOLEAN;

  function RISING_EDGE (signal S  : BOOLEAN) return BOOLEAN;
  function FALLING_EDGE (signal S : BOOLEAN) return BOOLEAN;

  function \?=\ (L, R  : BIT) return BIT;
  function \?/=\ (L, R : BIT) return BIT;
  function \?<\ (L, R  : BIT) return BIT;
  function \?<=\ (L, R : BIT) return BIT;
  function \?>\ (L, R  : BIT) return BIT;
  function \?>=\ (L, R : BIT) return BIT;

  function MINIMUM (L, R : BIT) return BIT;
  function MAXIMUM (L, R : BIT) return BIT;

  function \??\ (L : BIT) return BOOLEAN;

  function RISING_EDGE (signal S  : BIT) return BOOLEAN;
  function FALLING_EDGE (signal S : BIT) return BOOLEAN;

  function MINIMUM (L, R : CHARACTER) return CHARACTER;
  function MAXIMUM (L, R : CHARACTER) return CHARACTER;

  function MINIMUM (L, R : SEVERITY_LEVEL) return SEVERITY_LEVEL;
  function MAXIMUM (L, R : SEVERITY_LEVEL) return SEVERITY_LEVEL;

  function MINIMUM (L, R : INTEGER) return INTEGER;
  function MAXIMUM (L, R : INTEGER) return INTEGER;

  function MINIMUM (L, R : REAL) return REAL;
  function MAXIMUM (L, R : REAL) return REAL;

  function "mod" (L, R : TIME) return TIME;
  function "rem" (L, R : TIME) return TIME;

  function MINIMUM (L, R : TIME) return TIME;
  function MAXIMUM (L, R : TIME) return TIME;

  function MINIMUM (L, R : STRING) return STRING;
  function MAXIMUM (L, R : STRING) return STRING;

  function MINIMUM (L : STRING) return CHARACTER;
  function MAXIMUM (L : STRING) return CHARACTER;

  type BOOLEAN_VECTOR is array (NATURAL range <>) of BOOLEAN;

  -- The predefined operations for this type are as follows:

  function "and" (L, R  : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
  function "or" (L, R   : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
  function "nand" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
  function "nor" (L, R  : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
  function "xor" (L, R  : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
  function "xnor" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;

  function "not" (L : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;

  function "and" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR;
  function "and" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR;
  function "or" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR;
  function "or" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR;
  function "nand" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR;
  function "nand" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR;
  function "nor" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR;
  function "nor" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR;
  function "xor" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR;
  function "xor" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR;
  function "xnor" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR;
  function "xnor" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR;

  function and_reduce (L  : BOOLEAN_VECTOR) return BOOLEAN;
  function or_reduce (L  : BOOLEAN_VECTOR) return BOOLEAN;
  function nand_reduce (L  : BOOLEAN_VECTOR) return BOOLEAN;
  function nor_reduce (L  : BOOLEAN_VECTOR) return BOOLEAN;
  function xor_reduce (L  : BOOLEAN_VECTOR) return BOOLEAN;
  function xnor_reduce (L  : BOOLEAN_VECTOR) return BOOLEAN;

  function "sll" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR;
  function "srl" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR;
  function "sla" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR;
  function "sra" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR;
  function "rol" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR;
  function "ror" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR;

--  function "=" (L, R  : BOOLEAN_VECTOR) return BOOLEAN;
--  function "/=" (L, R : BOOLEAN_VECTOR) return BOOLEAN;
--  function "<" (L, R  : BOOLEAN_VECTOR) return BOOLEAN;
--  function "<=" (L, R : BOOLEAN_VECTOR) return BOOLEAN;
--  function ">" (L, R  : BOOLEAN_VECTOR) return BOOLEAN;
--  function ">=" (L, R : BOOLEAN_VECTOR) return BOOLEAN;

  function \?=\ (L, R  : BOOLEAN_VECTOR) return BOOLEAN;
  function \?/=\ (L, R : BOOLEAN_VECTOR) return BOOLEAN;

--  function "&" (L : BOOLEAN_VECTOR; R : BOOLEAN_VECTOR)
    -- return BOOLEAN_VECTOR;
--  function "&" (L : BOOLEAN_VECTOR; R : BOOLEAN) -- return BOOLEAN_VECTOR;
--  function "&" (L : BOOLEAN; R : BOOLEAN_VECTOR) -- return BOOLEAN_VECTOR;
--  function "&" (L : BOOLEAN; R : BOOLEAN) -- return BOOLEAN_VECTOR;

  function MINIMUM (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
  function MAXIMUM (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR;

  function MINIMUM (L : BOOLEAN_VECTOR) return BOOLEAN;
  function MAXIMUM (L : BOOLEAN_VECTOR) return BOOLEAN;

  function "and" (L  : BIT_VECTOR; R : BIT) return BIT_VECTOR;
  function "and" (L  : BIT; R : BIT_VECTOR) return BIT_VECTOR;
  function "or" (L   : BIT_VECTOR; R : BIT) return BIT_VECTOR;
  function "or" (L   : BIT; R : BIT_VECTOR) return BIT_VECTOR;
  function "nand" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR;
  function "nand" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR;
  function "nor" (L  : BIT_VECTOR; R : BIT) return BIT_VECTOR;
  function "nor" (L  : BIT; R : BIT_VECTOR) return BIT_VECTOR;
  function "xor" (L  : BIT_VECTOR; R : BIT) return BIT_VECTOR;
  function "xor" (L  : BIT; R : BIT_VECTOR) return BIT_VECTOR;
  function "xnor" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR;
  function "xnor" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR;

  function and_reduce (L  : BIT_VECTOR) return BIT;
  function or_reduce (L  : BIT_VECTOR) return BIT;
  function nand_reduce (L  : BIT_VECTOR) return BIT;
  function nor_reduce (L  : BIT_VECTOR) return BIT;
  function xor_reduce (L  : BIT_VECTOR) return BIT;
  function xnor_reduce (L  : BIT_VECTOR) return BIT;

  function \?=\ (L, R  : BIT_VECTOR) return BIT;
  function \?/=\ (L, R : BIT_VECTOR) return BIT;

  function MINIMUM (L, R : BIT_VECTOR) return BIT_VECTOR;
  function MAXIMUM (L, R : BIT_VECTOR) return BIT_VECTOR;

  function MINIMUM (L : BIT_VECTOR) return BIT;
  function MAXIMUM (L : BIT_VECTOR) return BIT;

  function TO_STRING (VALUE : BIT_VECTOR) return STRING;

  alias TO_BSTRING is TO_STRING [BIT_VECTOR return STRING];
  alias TO_BINARY_STRING is TO_STRING [BIT_VECTOR return STRING];
  function TO_OSTRING (VALUE : BIT_VECTOR) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [BIT_VECTOR return STRING];
  function TO_HSTRING (VALUE : BIT_VECTOR) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [BIT_VECTOR return STRING];

  type INTEGER_VECTOR is array (NATURAL range <>) of INTEGER;

  -- The predefined operations for this type are as follows:
  function "=" (L, R  : INTEGER_VECTOR) return BOOLEAN;
  function "/=" (L, R  : INTEGER_VECTOR) return BOOLEAN;
  function "<" (L, R  : INTEGER_VECTOR) return BOOLEAN;
  function "<=" (L, R  : INTEGER_VECTOR) return BOOLEAN;
  function ">" (L, R  : INTEGER_VECTOR) return BOOLEAN;
  function ">=" (L, R  : INTEGER_VECTOR) return BOOLEAN;

--  function "&" (L : INTEGER_VECTOR; R : INTEGER_VECTOR)
--    return INTEGER_VECTOR;
--  function "&" (L : INTEGER_VECTOR; R : INTEGER) return INTEGER_VECTOR;
--  function "&" (L : INTEGER; R : INTEGER_VECTOR) return INTEGER_VECTOR;
--  function "&" (L : INTEGER; R : INTEGER) return INTEGER_VECTOR;

  function MINIMUM (L, R : INTEGER_VECTOR) return INTEGER_VECTOR;
  function MAXIMUM (L, R : INTEGER_VECTOR) return INTEGER_VECTOR;

  function MINIMUM (L : INTEGER_VECTOR) return INTEGER;
  function MAXIMUM (L : INTEGER_VECTOR) return INTEGER;

  type REAL_VECTOR is array (NATURAL range <>) of REAL;

  -- The predefined operations for this type are as follows:
  function "=" (L, R  : REAL_VECTOR) return BOOLEAN;
  function "/=" (L, R  : REAL_VECTOR) return BOOLEAN;
  function "<" (L, R  : REAL_VECTOR) return BOOLEAN;
  function "<=" (L, R  : REAL_VECTOR) return BOOLEAN;
  function ">" (L, R  : REAL_VECTOR) return BOOLEAN;
  function ">=" (L, R  : REAL_VECTOR) return BOOLEAN;

--  function "&" (L : REAL_VECTOR; R : REAL_VECTOR)
--    return REAL_VECTOR;
--  function "&" (L : REAL_VECTOR; R : REAL) return REAL_VECTOR;
--  function "&" (L : REAL; R : REAL_VECTOR) return REAL_VECTOR;
--  function "&" (L : REAL; R : REAL) return REAL_VECTOR;

  function MINIMUM (L, R : REAL_VECTOR) return REAL_VECTOR;
  function MAXIMUM (L, R : REAL_VECTOR) return REAL_VECTOR;

  function MINIMUM (L : REAL_VECTOR) return REAL;
  function MAXIMUM (L : REAL_VECTOR) return REAL;

  type TIME_VECTOR is array (NATURAL range <>) of TIME;

  -- The predefined operations for this type are as follows:
  function "=" (L, R  : TIME_VECTOR) return BOOLEAN;
  function "/=" (L, R  : TIME_VECTOR) return BOOLEAN;
  function "<" (L, R  : TIME_VECTOR) return BOOLEAN;
  function "<=" (L, R  : TIME_VECTOR) return BOOLEAN;
  function ">" (L, R  : TIME_VECTOR) return BOOLEAN;
  function ">=" (L, R  : TIME_VECTOR) return BOOLEAN;

--  function "&" (L : TIME_VECTOR; R : TIME_VECTOR)
--    return TIME_VECTOR;
--  function "&" (L : TIME_VECTOR; R : TIME) return TIME_VECTOR;
--  function "&" (L : TIME; R : TIME_VECTOR) return TIME_VECTOR;
--  function "&" (L : TIME; R : TIME) return TIME_VECTOR;

  function MINIMUM (L, R : TIME_VECTOR) return TIME_VECTOR;
  function MAXIMUM (L, R : TIME_VECTOR) return TIME_VECTOR;

  function MINIMUM (L : TIME_VECTOR) return TIME;
  function MAXIMUM (L : TIME_VECTOR) return TIME;

  function MINIMUM (L, R : FILE_OPEN_KIND) return FILE_OPEN_KIND;
  function MAXIMUM (L, R : FILE_OPEN_KIND) return FILE_OPEN_KIND;

  function MINIMUM (L, R : FILE_OPEN_STATUS) return FILE_OPEN_STATUS;
  function MAXIMUM (L, R : FILE_OPEN_STATUS) return FILE_OPEN_STATUS;

  -- predefined TO_STRING operations on scalar types
  function TO_STRING (VALUE : BOOLEAN) return STRING;
  function TO_STRING (VALUE : BIT) return STRING;
  function TO_STRING (VALUE : CHARACTER) return STRING;
  function TO_STRING (VALUE : SEVERITY_LEVEL) return STRING;
  function TO_STRING (VALUE : INTEGER) return STRING;
  function TO_STRING (VALUE : REAL) return STRING;
  function TO_STRING (VALUE : TIME) return STRING;
  function TO_STRING (VALUE : FILE_OPEN_KIND) return STRING;
  function TO_STRING (VALUE : FILE_OPEN_STATUS) return STRING;

  -- predefined overloaded TO_STRING operations
  function TO_STRING (VALUE : REAL; DIGITS : NATURAL) return STRING;
  function TO_STRING (VALUE : REAL; FORMAT : STRING) return STRING;
  function TO_STRING (VALUE : TIME; UNIT : TIME) return STRING;
end package standard_additions;

------------------------------------------------------------------------------
-- "standard_additions" package contains the additions to the built in
-- "standard.std" package.  In the final version this package will be implicit.
--  Created for VHDL-200X par, David Bishop (dbishop@vhdl.org)
------------------------------------------------------------------------------
use std.textio.all;
package body standard_additions is

  function \?=\ (L, R : BOOLEAN) return BOOLEAN is
  begin
    return L = R;
  end function \?=\;

  function \?/=\ (L, R : BOOLEAN) return BOOLEAN is
  begin
    return L /= R;
  end function \?/=\;

  function \?<\ (L, R : BOOLEAN) return BOOLEAN is
  begin
    return L < R;
  end function \?<\;

  function \?<=\ (L, R : BOOLEAN) return BOOLEAN is
  begin
    return L <= R;
  end function \?<=\;

  function \?>\ (L, R : BOOLEAN) return BOOLEAN is
  begin
    return L > R;
  end function \?>\;

  function \?>=\ (L, R : BOOLEAN) return BOOLEAN is
  begin
    return L >= R;
  end function \?>=\;

  function MINIMUM (L, R : BOOLEAN) return BOOLEAN is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;
  function MAXIMUM (L, R : BOOLEAN) return BOOLEAN is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : BOOLEAN) return STRING is
  begin
    return BOOLEAN'image(VALUE);
  end function TO_STRING;

  function RISING_EDGE (signal S : BOOLEAN) return BOOLEAN is
  begin
    return (s'event and (s = true) and (s'last_value = false));
  end function rising_edge;

  function FALLING_EDGE (signal S : BOOLEAN) return BOOLEAN is
  begin
    return (s'event and (s = false) and (s'last_value = true));
  end function falling_edge;

  function \?=\ (L, R : BIT) return BIT is
  begin
    if L = R then
      return '1';
    else
      return '0';
    end if;
  end function \?=\;

  function \?/=\ (L, R : BIT) return BIT is
  begin
    if L /= R then
      return '1';
    else
      return '0';
    end if;
  end function \?/=\;

  function \?<\ (L, R : BIT) return BIT is
  begin
    if L < R then
      return '1';
    else
      return '0';
    end if;
  end function \?<\;

  function \?<=\ (L, R : BIT) return BIT is
  begin
    if L <= R then
      return '1';
    else
      return '0';
    end if;
  end function \?<=\;

  function \?>\ (L, R : BIT) return BIT is
  begin
    if L > R then
      return '1';
    else
      return '0';
    end if;
  end function \?>\;

  function \?>=\ (L, R : BIT) return BIT is
  begin
    if L >= R then
      return '1';
    else
      return '0';
    end if;
  end function \?>=\;

  function MINIMUM (L, R : BIT) return BIT is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : BIT) return BIT is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : BIT) return STRING is
  begin
    if VALUE = '1' then
      return "1";
    else
      return "0";
    end if;
  end function TO_STRING;

  function \??\ (L : BIT) return BOOLEAN is
  begin
    return L = '1';
  end function \??\;

  function RISING_EDGE (signal S : BIT) return BOOLEAN is
  begin
    return (s'event and (s = '1') and (s'last_value = '0'));
  end function rising_edge;

  function FALLING_EDGE (signal S : BIT) return BOOLEAN is
  begin
    return (s'event and (s = '0') and (s'last_value = '1'));
  end function falling_edge;

  function MINIMUM (L, R : CHARACTER) return CHARACTER is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : CHARACTER) return CHARACTER is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : CHARACTER) return STRING is
    variable result : STRING (1 to 1);
  begin
    result (1) := VALUE;
    return result;
  end function TO_STRING;

  function MINIMUM (L, R : SEVERITY_LEVEL) return SEVERITY_LEVEL is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : SEVERITY_LEVEL) return SEVERITY_LEVEL is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : SEVERITY_LEVEL) return STRING is
  begin
    return SEVERITY_LEVEL'image(VALUE);
  end function TO_STRING;

  function MINIMUM (L, R : INTEGER) return INTEGER is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : INTEGER) return INTEGER is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : INTEGER) return STRING is
  begin
    return INTEGER'image(VALUE);
  end function TO_STRING;

  function MINIMUM (L, R : REAL) return REAL is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : REAL) return REAL is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : REAL) return STRING is
  begin
    return REAL'image (VALUE);
  end function TO_STRING;

  function TO_STRING (VALUE : REAL; DIGITS : NATURAL) return STRING is
  begin
    return to_string (VALUE, "%1." & INTEGER'image(DIGITS) & "f");
  end function TO_STRING;

  function "mod" (L, R : TIME) return TIME is
    variable lint, rint : INTEGER;
  begin
    lint := L / 1.0 ns;
    rint := R / 1.0 ns;
    return (lint mod rint) * 1.0 ns;
  end function "mod";

  function "rem" (L, R : TIME) return TIME is
    variable lint, rint : INTEGER;
  begin
    lint := L / 1.0 ns;
    rint := R / 1.0 ns;
    return (lint rem rint) * 1.0 ns;
  end function "rem";

  function MINIMUM (L, R : TIME) return TIME is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : TIME) return TIME is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : TIME) return STRING is
  begin
    return TIME'image (VALUE);
  end function TO_STRING;

  function MINIMUM (L, R : STRING) return STRING is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : STRING) return STRING is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function MINIMUM (L : STRING) return CHARACTER is
    variable result : CHARACTER := CHARACTER'high;
  begin
    for i in l'range loop
      result := minimum (l(i), result);
    end loop;
    return result;
  end function MINIMUM;

  function MAXIMUM (L : STRING) return CHARACTER is
    variable result : CHARACTER := CHARACTER'low;
  begin
    for i in l'range loop
      result := maximum (l(i), result);
    end loop;
    return result;
  end function MAXIMUM;

  -- type BOOLEAN_VECTOR is array (NATURAL range <>) of BOOLEAN;
  -- The predefined operations for this type are as follows:
  function "and" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD.""and"": "
        & "arguments of overloaded 'and' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := (lv(i) and rv(i));
      end loop;
    end if;
    return result;
  end function "and";

  function "or" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD.""or"": "
        & "arguments of overloaded 'or' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := (lv(i) or rv(i));
      end loop;
    end if;
    return result;
  end function "or";

  function "nand" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD.""nand"": "
        & "arguments of overloaded 'nand' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := (lv(i) nand rv(i));
      end loop;
    end if;
    return result;
  end function "nand";

  function "nor" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD.""nor"": "
        & "arguments of overloaded 'nor' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := (lv(i) nor rv(i));
      end loop;
    end if;
    return result;
  end function "nor";

  function "xor" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD.""xor"": "
        & "arguments of overloaded 'xor' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := (lv(i) xor rv(i));
      end loop;
    end if;
    return result;
  end function "xor";

  function "xnor" (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD.""xnor"": "
        & "arguments of overloaded 'xnor' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := (lv(i) xnor rv(i));
      end loop;
    end if;
    return result;
  end function "xnor";

  function "not" (L : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := not (lv(i));
    end loop;
    return result;
  end function "not";

  function "and" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) and r;
    end loop;
    return result;
  end function "and";

  function "and" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR is
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l and rv(i);
    end loop;
    return result;
  end function "and";

  function "or" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) or r;
    end loop;
    return result;
  end function "or";

  function "or" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR is
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l or rv(i);
    end loop;
    return result;
  end function "or";

  function "nand" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) nand r;
    end loop;
    return result;
  end function "nand";

  function "nand" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR is
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l nand rv(i);
    end loop;
    return result;
  end function "nand";

  function "nor" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) nor r;
    end loop;
    return result;
  end function "nor";

  function "nor" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR is
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l nor rv(i);
    end loop;
    return result;
  end function "nor";

  function "xor" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) xor r;
    end loop;
    return result;
  end function "xor";

  function "xor" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR is
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l xor rv(i);
    end loop;
    return result;
  end function "xor";

  function "xnor" (L : BOOLEAN_VECTOR; R : BOOLEAN)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) xnor r;
    end loop;
    return result;
  end function "xnor";

  function "xnor" (L : BOOLEAN; R : BOOLEAN_VECTOR)
    return BOOLEAN_VECTOR is
    alias rv : BOOLEAN_VECTOR (1 to r'length) is r;
    variable result : BOOLEAN_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l xnor rv(i);
    end loop;
    return result;
  end function "xnor";

  function and_reduce (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := true;
  begin
    for i in l'reverse_range loop
      result := l(i) and result;
    end loop;
    return result;
  end function and_reduce;

  function or_reduce (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := false;
  begin
    for i in l'reverse_range loop
      result := l(i) or result;
    end loop;
    return result;
  end function or_reduce;

  function nand_reduce (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := true;
  begin
    for i in l'reverse_range loop
      result := l(i) and result;
    end loop;
    return not result;
  end function nand_reduce;

  function nor_reduce (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := false;
  begin
    for i in l'reverse_range loop
      result := l(i) or result;
    end loop;
    return not result;
  end function nor_reduce;

  function xor_reduce (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := false;
  begin
    for i in l'reverse_range loop
      result := l(i) xor result;
    end loop;
    return result;
  end function xor_reduce;

  function xnor_reduce (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := false;
  begin
    for i in l'reverse_range loop
      result := l(i) xor result;
    end loop;
    return not result;
  end function xnor_reduce;

  function "sll" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if r >= 0 then
      result(1 to l'length - r) := lv(r + 1 to l'length);
    else
      result := l srl -r;
    end if;
    return result;
  end function "sll";

  function "srl" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    if r >= 0 then
      result(r + 1 to l'length) := lv(1 to l'length - r);
    else
      result := l sll -r;
    end if;
    return result;
  end function "srl";

  function "sla" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in L'range loop
      result (i) := L(L'high);
    end loop;
    if r >= 0 then
      result(1 to l'length - r) := lv(r + 1 to l'length);
    else
      result := l sra -r;
    end if;
    return result;
  end function "sla";

  function "sra" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
  begin
    for i in L'range loop
      result (i) := L(L'low);
    end loop;
    if r >= 0 then
      result(1 to l'length - r) := lv(r + 1 to l'length);
    else
      result := l sra -r;
    end if;
    return result;
  end function "sra";

  function "rol" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
    constant rm : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(1 to l'length - rm) := lv(rm + 1 to l'length);
      result(l'length - rm + 1 to l'length) := lv(1 to rm);
    else
      result := l ror -r;
    end if;
    return result;
  end function "rol";

  function "ror" (L : BOOLEAN_VECTOR; R : INTEGER)
    return BOOLEAN_VECTOR is
    alias lv : BOOLEAN_VECTOR (1 to l'length) is l;
    variable result : BOOLEAN_VECTOR (1 to l'length);
    constant rm : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(rm + 1 to l'length) := lv(1 to l'length - rm);
      result(1 to rm) := lv(l'length - rm + 1 to l'length);
    else
      result := l rol -r;
    end if;
    return result;
  end function "ror";
--  function "=" (L, R: BOOLEAN_VECTOR) return BOOLEAN;
--  function "/=" (L, R: BOOLEAN_VECTOR) return BOOLEAN;
--  function "<" (L, R: BOOLEAN_VECTOR) return BOOLEAN;
--  function "<=" (L, R: BOOLEAN_VECTOR) return BOOLEAN;
--  function ">" (L, R: BOOLEAN_VECTOR) return BOOLEAN;
--  function ">=" (L, R: BOOLEAN_VECTOR) return BOOLEAN;

  function \?=\ (L, R : BOOLEAN_VECTOR) return BOOLEAN is
  begin
    return L = R;
  end function \?=\;

  function \?/=\ (L, R : BOOLEAN_VECTOR) return BOOLEAN is
  begin
    return L /= R;
  end function \?/=\;
--  function "&" (L: BOOLEAN_VECTOR; R: BOOLEAN_VECTOR)
--    return BOOLEAN_VECTOR;
--  function "&" (L: BOOLEAN_VECTOR; R: BOOLEAN) return BOOLEAN_VECTOR;
--  function "&" (L: BOOLEAN; R: BOOLEAN_VECTOR) return BOOLEAN_VECTOR;
--  function "&" (L: BOOLEAN; R: BOOLEAN) return BOOLEAN_VECTOR;

  function MINIMUM (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : BOOLEAN_VECTOR) return BOOLEAN_VECTOR is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function MINIMUM (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := BOOLEAN'high;
  begin
    for i in l'range loop
      result := minimum (l(i), result);
    end loop;
    return result;
  end function MINIMUM;

  function MAXIMUM (L : BOOLEAN_VECTOR) return BOOLEAN is
    variable result : BOOLEAN := BOOLEAN'low;
  begin
    for i in l'range loop
      result := maximum (l(i), result);
    end loop;
    return result;
  end function MAXIMUM;

  function "and" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
    alias lv : BIT_VECTOR (1 to l'length) is l;
    variable result : BIT_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) and r;
    end loop;
    return result;
  end function "and";

  function "and" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
    alias rv : BIT_VECTOR (1 to r'length) is r;
    variable result : BIT_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l and rv(i);
    end loop;
    return result;
  end function "and";

  function "or" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
    alias lv : BIT_VECTOR (1 to l'length) is l;
    variable result : BIT_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) or r;
    end loop;
    return result;
  end function "or";

  function "or" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
    alias rv : BIT_VECTOR (1 to r'length) is r;
    variable result : BIT_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l or rv(i);
    end loop;
    return result;
  end function "or";

  function "nand" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
    alias lv : BIT_VECTOR (1 to l'length) is l;
    variable result : BIT_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) and r;
    end loop;
    return not result;
  end function "nand";

  function "nand" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
    alias rv : BIT_VECTOR (1 to r'length) is r;
    variable result : BIT_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l and rv(i);
    end loop;
    return not result;
  end function "nand";

  function "nor" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
    alias lv : BIT_VECTOR (1 to l'length) is l;
    variable result : BIT_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) or r;
    end loop;
    return not result;
  end function "nor";

  function "nor" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
    alias rv : BIT_VECTOR (1 to r'length) is r;
    variable result : BIT_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l or rv(i);
    end loop;
    return not result;
  end function "nor";

  function "xor" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
    alias lv : BIT_VECTOR (1 to l'length) is l;
    variable result : BIT_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) xor r;
    end loop;
    return result;
  end function "xor";

  function "xor" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
    alias rv : BIT_VECTOR (1 to r'length) is r;
    variable result : BIT_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l xor rv(i);
    end loop;
    return result;
  end function "xor";

  function "xnor" (L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
    alias lv : BIT_VECTOR (1 to l'length) is l;
    variable result : BIT_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := lv(i) xor r;
    end loop;
    return not result;
  end function "xnor";

  function "xnor" (L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
    alias rv : BIT_VECTOR (1 to r'length) is r;
    variable result : BIT_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := l xor rv(i);
    end loop;
    return not result;
  end function "xnor";

  function and_reduce (L : BIT_VECTOR) return BIT is
    variable result : BIT := '1';
  begin
    for i in l'reverse_range loop
      result := l(i) and result;
    end loop;
    return result;
  end function and_reduce;

  function or_reduce (L : BIT_VECTOR) return BIT is
    variable result : BIT := '0';
  begin
    for i in l'reverse_range loop
      result := l(i) or result;
    end loop;
    return result;
  end function or_reduce;

  function nand_reduce (L : BIT_VECTOR) return BIT is
    variable result : BIT := '1';
  begin
    for i in l'reverse_range loop
      result := l(i) and result;
    end loop;
    return not result;
  end function nand_reduce;

  function nor_reduce (L : BIT_VECTOR) return BIT is
    variable result : BIT := '0';
  begin
    for i in l'reverse_range loop
      result := l(i) or result;
    end loop;
    return not result;
  end function nor_reduce;

  function xor_reduce (L : BIT_VECTOR) return BIT is
    variable result : BIT := '0';
  begin
    for i in l'reverse_range loop
      result := l(i) xor result;
    end loop;
    return result;
  end function xor_reduce;

  function xnor_reduce (L : BIT_VECTOR) return BIT is
    variable result : BIT := '0';
  begin
    for i in l'reverse_range loop
      result := l(i) xor result;
    end loop;
    return not result;
  end function xnor_reduce;

  function \?=\ (L, R : BIT_VECTOR) return BIT is
  begin
    if L = R then
      return '1';
    else
      return '0';
    end if;
  end function \?=\;

  function \?/=\ (L, R : BIT_VECTOR) return BIT is
  begin
    if L /= R then
      return '1';
    else
      return '0';
    end if;
  end function \?/=\;

  function MINIMUM (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function MINIMUM (L : BIT_VECTOR) return BIT is
    variable result : BIT := BIT'high;
  begin
    for i in l'range loop
      result := minimum (l(i), result);
    end loop;
    return result;
  end function MINIMUM;

  function MAXIMUM (L : BIT_VECTOR) return BIT is
    variable result : BIT := BIT'low;
  begin
    for i in l'range loop
      result := maximum (l(i), result);
    end loop;
    return result;
  end function MAXIMUM;

  function TO_STRING (VALUE : BIT_VECTOR) return STRING is
    alias ivalue : BIT_VECTOR(1 to value'length) is value;
    variable result : STRING(1 to value'length);
  begin
    if value'length < 1 then
      return "";
    else
      for i in ivalue'range loop
        if iValue(i) = '0' then
          result(i) := '0';
        else
          result(i) := '1';
        end if;
      end loop;
      return result;
    end if;
  end function to_string;

--  alias TO_BSTRING is TO_STRING [BIT_VECTOR return STRING];
--  alias TO_BINARY_STRING is TO_STRING [BIT_VECTOR return STRING];

  function TO_OSTRING (VALUE : BIT_VECTOR) return STRING is
    constant ne : INTEGER := (value'length+2)/3;
    constant pad : BIT_VECTOR(0 to (ne*3 - value'length) - 1) := (others => '0');
    variable ivalue : BIT_VECTOR(0 to ne*3 - 1);
    variable result : STRING(1 to ne);
    variable tri : BIT_VECTOR(0 to 2);
  begin
    if value'length < 1 then
      return "";
    end if;
    ivalue := pad & value;
    for i in 0 to ne-1 loop
      tri := ivalue(3*i to 3*i+2);
      case tri is
        when o"0" => result(i+1) := '0';
        when o"1" => result(i+1) := '1';
        when o"2" => result(i+1) := '2';
        when o"3" => result(i+1) := '3';
        when o"4" => result(i+1) := '4';
        when o"5" => result(i+1) := '5';
        when o"6" => result(i+1) := '6';
        when o"7" => result(i+1) := '7';
      end case;
    end loop;
    return result;
  end function to_ostring;
--  alias TO_OCTAL_STRING is TO_OSTRING [BIT_VECTOR return STRING];

  function TO_HSTRING (VALUE : BIT_VECTOR) return STRING is
    constant ne : INTEGER := (value'length+3)/4;
    constant pad : BIT_VECTOR(0 to (ne*4 - value'length) - 1) := (others => '0');
    variable ivalue : BIT_VECTOR(0 to ne*4 - 1);
    variable result : STRING(1 to ne);
    variable quad : BIT_VECTOR(0 to 3);
  begin
    if value'length < 1 then
      return "";
    end if;
    ivalue := pad & value;
    for i in 0 to ne-1 loop
      quad := ivalue(4*i to 4*i+3);
      case quad is
        when x"0" => result(i+1) := '0';
        when x"1" => result(i+1) := '1';
        when x"2" => result(i+1) := '2';
        when x"3" => result(i+1) := '3';
        when x"4" => result(i+1) := '4';
        when x"5" => result(i+1) := '5';
        when x"6" => result(i+1) := '6';
        when x"7" => result(i+1) := '7';
        when x"8" => result(i+1) := '8';
        when x"9" => result(i+1) := '9';
        when x"A" => result(i+1) := 'A';
        when x"B" => result(i+1) := 'B';
        when x"C" => result(i+1) := 'C';
        when x"D" => result(i+1) := 'D';
        when x"E" => result(i+1) := 'E';
        when x"F" => result(i+1) := 'F';
      end case;
    end loop;
    return result;
  end function to_hstring;
--  alias TO_HEX_STRING is TO_HSTRING [BIT_VECTOR return STRING];

--  type INTEGER_VECTOR is array (NATURAL range <>) of INTEGER;
  -- The predefined operations for this type are as follows:

  function "=" (L, R : INTEGER_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length or L'length < 1 or R'length < 1 then
      return false;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          return false;
        end if;
      end loop;
      return true;
    end if;
  end function "=";

  function "/=" (L, R : INTEGER_VECTOR) return BOOLEAN is
  begin
    return not (L = R);
  end function "/=";

  function "<" (L, R : INTEGER_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length < R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) < R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return false;
    end if;
  end function "<";

  function "<=" (L, R : INTEGER_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length < R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) < R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return true;
    end if;
  end function "<=";

  function ">" (L, R : INTEGER_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length > R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) > R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return false;
    end if;
  end function ">";

  function ">=" (L, R : INTEGER_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length > R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) > R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return true;
    end if;
  end function ">=";
--  function "&" (L: INTEGER_VECTOR; R: INTEGER_VECTOR)
--    return INTEGER_VECTOR;
--  function "&" (L: INTEGER_VECTOR; R: INTEGER) return INTEGER_VECTOR;
--  function "&" (L: INTEGER; R: INTEGER_VECTOR) return INTEGER_VECTOR;
--  function "&" (L: INTEGER; R: INTEGER) return INTEGER_VECTOR;

  function MINIMUM (L, R : INTEGER_VECTOR) return INTEGER_VECTOR is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : INTEGER_VECTOR) return INTEGER_VECTOR is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function MINIMUM (L : INTEGER_VECTOR) return INTEGER is
    variable result : INTEGER := INTEGER'high;
  begin
    for i in l'range loop
      result := minimum (l(i), result);
    end loop;
    return result;
  end function MINIMUM;

  function MAXIMUM (L : INTEGER_VECTOR) return INTEGER is
    variable result : INTEGER := INTEGER'low;
  begin
    for i in l'range loop
      result := maximum (l(i), result);
    end loop;
    return result;
  end function MAXIMUM;

  -- type REAL_VECTOR is array (NATURAL range <>) of REAL;
  -- The predefined operations for this type are as follows:
  function "=" (L, R : REAL_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length or L'length < 1 or R'length < 1 then
      return false;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          return false;
        end if;
      end loop;
      return true;
    end if;
  end function "=";

  function "/=" (L, R : REAL_VECTOR) return BOOLEAN is
  begin
    return not (L = R);
  end function "/=";

  function "<" (L, R : REAL_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length < R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) < R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return false;
    end if;
  end function "<";

  function "<=" (L, R : REAL_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length < R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) < R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return true;
    end if;
  end function "<=";

  function ">" (L, R : REAL_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length > R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) > R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return false;
    end if;
  end function ">";

  function ">=" (L, R : REAL_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length > R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) > R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return true;
    end if;
  end function ">=";
--  function "&" (L: REAL_VECTOR; R: REAL_VECTOR)
--    return REAL_VECTOR;
--  function "&" (L: REAL_VECTOR; R: REAL) return REAL_VECTOR;
--  function "&" (L: REAL; R: REAL_VECTOR) return REAL_VECTOR;
--  function "&" (L: REAL; R: REAL) return REAL_VECTOR;

  function MINIMUM (L, R : REAL_VECTOR) return REAL_VECTOR is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : REAL_VECTOR) return REAL_VECTOR is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function MINIMUM (L : REAL_VECTOR) return REAL is
    variable result : REAL := REAL'high;
  begin
    for i in l'range loop
      result := minimum (l(i), result);
    end loop;
    return result;
  end function MINIMUM;

  function MAXIMUM (L : REAL_VECTOR) return REAL is
    variable result : REAL := REAL'low;
  begin
    for i in l'range loop
      result := maximum (l(i), result);
    end loop;
    return result;
  end function MAXIMUM;

  -- type TIME_VECTOR is array (NATURAL range <>) of TIME;
  -- The predefined implicit operations for this type are as follows:
  function "=" (L, R : TIME_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length or L'length < 1 or R'length < 1 then
      return false;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          return false;
        end if;
      end loop;
      return true;
    end if;
  end function "=";

  function "/=" (L, R : TIME_VECTOR) return BOOLEAN is
  begin
    return not (L = R);
  end function "/=";

  function "<" (L, R : TIME_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length < R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) < R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return false;
    end if;
  end function "<";

  function "<=" (L, R : TIME_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length < R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) < R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return true;
    end if;
  end function "<=";

  function ">" (L, R : TIME_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length > R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) > R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return false;
    end if;
  end function ">";

  function ">=" (L, R : TIME_VECTOR) return BOOLEAN is
  begin
    if L'length /= R'length then
      return L'length > R'length;
    else
      for i in l'range loop
        if L(i) /= R(i) then
          if L(i) > R(i) then
            return true;
          else
            return false;
          end if;
        end if;
      end loop;
      return true;
    end if;
  end function ">=";
--  function "&" (L: TIME_VECTOR; R: TIME_VECTOR)
--    return TIME_VECTOR;
--  function "&" (L: TIME_VECTOR; R: TIME) return TIME_VECTOR;
--  function "&" (L: TIME; R: TIME_VECTOR) return TIME_VECTOR;
--  function "&" (L: TIME; R: TIME) return TIME_VECTOR;

  function MINIMUM (L, R : TIME_VECTOR) return TIME_VECTOR is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : TIME_VECTOR) return TIME_VECTOR is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function MINIMUM (L : TIME_VECTOR) return TIME is
    variable result : TIME := TIME'high;
  begin
    for i in l'range loop
      result := minimum (l(i), result);
    end loop;
    return result;
  end function MINIMUM;

  function MAXIMUM (L : TIME_VECTOR) return TIME is
    variable result : TIME := TIME'low;
  begin
    for i in l'range loop
      result := maximum (l(i), result);
    end loop;
    return result;
  end function MAXIMUM;

  function MINIMUM (L, R : FILE_OPEN_KIND) return FILE_OPEN_KIND is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : FILE_OPEN_KIND) return FILE_OPEN_KIND is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : FILE_OPEN_KIND) return STRING is
  begin
    return FILE_OPEN_KIND'image(VALUE);
  end function TO_STRING;

  function MINIMUM (L, R : FILE_OPEN_STATUS) return FILE_OPEN_STATUS is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function MINIMUM;

  function MAXIMUM (L, R : FILE_OPEN_STATUS) return FILE_OPEN_STATUS is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function MAXIMUM;

  function TO_STRING (VALUE : FILE_OPEN_STATUS) return STRING is
  begin
    return FILE_OPEN_STATUS'image(VALUE);
  end function TO_STRING;

  -- USED INTERNALLY!
  function justify (
    value : in STRING;
    justified : in SIDE := right;
    field : in width := 0)
    return STRING is
    constant VAL_LEN : INTEGER := value'length;
    variable result : STRING (1 to field) := (others => ' ');
  begin  -- function justify
    -- return value if field is too small
    if VAL_LEN >= field then
      return value;
    end if;
    if justified = left then
      result(1 to VAL_LEN) := value;
    elsif justified = right then
      result(field - VAL_LEN + 1 to field) := value;
    end if;
    return result;
  end function justify;

  function TO_STRING (VALUE : TIME; UNIT : TIME) return STRING is
    variable L : LINE;  -- pointer
  begin
    deallocate (L);
    write (L => L,
           VALUE => VALUE,
           UNIT => UNIT);
    return L.all;
  end function to_string;

  function TO_STRING (VALUE : REAL; FORMAT : STRING) return STRING is
    constant czero : CHARACTER := '0';  -- zero
    constant half : REAL := 0.4999999999;  -- almost 0.5
    -- Log10 funciton
    function log10 (arg : REAL) return INTEGER is
      variable i : INTEGER := 1;
    begin
      if ((arg = 0.0)) then
        return 0;
      elsif arg >= 1.0 then
        while arg >= 10.0**i loop
          i := i + 1;
        end loop;
        return (i-1);
      else
        while arg < 10.0**i loop
          i := i - 1;
        end loop;
        return i;
      end if;
    end function log10;
    -- purpose: writes a fractional real number into a line
    procedure writefrc (
      variable L : inout LINE;  -- LINE
      variable cdes : in CHARACTER;
      variable precision : in INTEGER;  -- number of decimal places
      variable value : in REAL) is  -- real value
      variable rvar : REAL;  -- temp variable
      variable xint : INTEGER;
      variable xreal : REAL;
    begin
      xreal := (10.0**(-precision));
      write (L, '.');
      rvar := value;
      for i in 1 to precision loop
        rvar := rvar * 10.0;
        xint := INTEGER(rvar-0.49999999999);  -- round
        write (L, xint);
        rvar := rvar - REAL(xint);
        xreal := xreal * 10.0;
        if (cdes = 'g') and (rvar < xreal) then
          exit;
        end if;
      end loop;
    end procedure writefrc;
    -- purpose: replace the "." with a "@", and "e" with "j" to get around
    -- read ("6.") and read ("2e") issues.
    function subdot (
      constant format : STRING)
      return STRING is
      variable result : STRING (format'range);
    begin
      for i in format'range loop
        if (format(i) = '.') then
          result(i) := '@';  -- Because the parser reads 6.2 as REAL
        elsif (format(i) = 'e') then
          result(i) := 'j';  -- Because the parser read 2e as REAL
        elsif (format(i) = 'E') then
          result(i) := 'J';  -- Because the parser reads 2E as REAL
        else
          result(i) := format(i);
        end if;
      end loop;
      return result;
    end function subdot;
    -- purpose: find a . in a STRING
    function isdot (
      constant format : STRING)
      return BOOLEAN is
    begin
      for i in format'range loop
        if (format(i) = '@') then
          return true;
        end if;
      end loop;
      return false;
    end function isdot;
    variable exp : INTEGER;  -- integer version of baseexp
    variable bvalue : REAL;  -- base value
    variable roundvar, tvar : REAL;  -- Rounding values
    variable frcptr : INTEGER;  -- integer version of number
    variable fwidth, dwidth : INTEGER;  -- field width and decimal width
    variable dash, dot : BOOLEAN := false;
    variable cdes, ddes : CHARACTER := ' ';
    variable L : LINE;  -- line type
  begin
    -- Perform the same function that "printf" does
    -- examples "%6.2f" "%-7e" "%g"
    if not (format(format'left) = '%') then
      report "to_string: Illegal format string """ & format & '"'
        severity error;
      return "";
    end if;
    L := new STRING'(subdot(format));
    read (L, ddes);  -- toss the '%'
    case L.all(1) is
      when '-' => dash := true;
      when '@' => dash := true;  -- in FP, a "-" and a "." are the same
      when 'f' => cdes := 'f';
      when 'F' => cdes := 'F';
      when 'g' => cdes := 'g';
      when 'G' => cdes := 'G';
      when 'j' => cdes := 'e';  -- parser reads 5e as real, thus we sub j
      when 'J' => cdes := 'E';
      when '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => null;
      when others =>
        report "to_string: Illegal format string """ & format & '"'
          severity error;
        return "";
    end case;
    if (dash or (cdes /= ' ')) then
      read (L, ddes);  -- toss the next character
    end if;
    if (cdes = ' ') then
      if (isdot(L.all)) then  -- if you see a . two numbers
        read (L, fwidth);  -- read field width
        read (L, ddes);  -- toss the next character .
        read (L, dwidth);  -- read decimal width       
      else
        read (L, fwidth);  -- read field width
        dwidth := 6;  -- the default decimal width is 6
      end if;
      read (L, cdes);
      if (cdes = 'j') then
        cdes := 'e';  -- because 2e reads as "REAL".
      elsif (cdes = 'J') then
        cdes := 'E';
      end if;
    else
      if (cdes = 'E' or cdes = 'e') then
        fwidth := 10;  -- default for e and E is %10.6e
      else
        fwidth := 0;  -- default for f and g is %0.6f
      end if;
      dwidth := 6;
    end if;
    deallocate (L);  -- reclame the pointer L.
--      assert (not debug) report "Format: " & format & " "
--        & INTEGER'image(fwidth) & "." & INTEGER'image(dwidth) & cdes
--        severity note;
    if (not (cdes = 'f' or cdes = 'F' or cdes = 'g' or cdes = 'G'
             or cdes = 'e' or cdes = 'E')) then
      report "to_string: Illegal format """ & format & '"' severity error;   
      return "";
    end if;
    if (VALUE < 0.0) then
      bvalue := -value;
      write (L, '-');
    else
      bvalue := value;
    end if;
    case cdes is
      when 'e' | 'E' =>  -- 7.000E+01
        exp := log10(bvalue);
        roundvar := half*(10.0**(exp-dwidth));
        bvalue := bvalue + roundvar;  -- round
        exp := log10(bvalue);  -- because we CAN overflow
        bvalue := bvalue * (10.0**(-exp));  -- result is D.XXXXXX
        frcptr := INTEGER(bvalue-half);  -- Write a single digit.
        write (L, frcptr);
        bvalue := bvalue - REAL(frcptr);
        writefrc (-- Write out the fraction
          L => L,
          cdes => cdes,
          precision => dwidth,
          value => bvalue);
        write (L, cdes);  -- e or E
        if (exp < 0) then
          write (L, '-');
        else
          write (L, '+');
        end if;
        exp := abs(exp);
        if (exp < 10) then  -- we need another "0".
          write (L, czero);
        end if;
        write (L, exp);
      when 'f' | 'F' =>  -- 70.0
        exp := log10(bvalue);
        roundvar := half*(10.0**(-dwidth));
        bvalue := bvalue + roundvar;  -- round
        exp := log10(bvalue);  -- because we CAN overflow
        if (exp < 0) then  -- 0.X case
          write (L, czero);
        else  -- loop because real'high > integer'high
          while (exp >= 0) loop
            frcptr := INTEGER(bvalue * (10.0**(-exp)) - half);
            write (L, frcptr);
            bvalue := bvalue - (REAL(frcptr) * (10.0**exp));
            exp := exp-1;
          end loop;
        end if;
        writefrc (
          L => L,
          cdes => cdes,
          precision => dwidth,
          value => bvalue);
      when 'g' | 'G' =>  -- 70
        exp := log10(bvalue);
        roundvar := half*(10.0**(exp-dwidth));  -- small number
        bvalue := bvalue + roundvar;  -- round
        exp := log10(bvalue);  -- because we CAN overflow
        frcptr := INTEGER(bvalue-half);
        tvar := bvalue-roundvar - REAL(frcptr);  -- even smaller number
        if (exp < dwidth)
          and (tvar < roundvar and tvar > -roundvar) then
--            and ((bvalue-roundvar) = real(frcptr)) then
          write (L, frcptr);  -- Just a short integer, write it.
        elsif (exp >= dwidth) or (exp < -4) then
          -- in "e" format (modified)
          bvalue := bvalue * (10.0**(-exp));  -- result is D.XXXXXX
          frcptr := INTEGER(bvalue-half);
          write (L, frcptr);
          bvalue := bvalue - REAL(frcptr);
          if (bvalue > (10.0**(1-dwidth))) then
            dwidth := dwidth - 1;
            writefrc (
              L => L,
              cdes => cdes,
              precision => dwidth,
              value => bvalue);
          end if;
          if (cdes = 'G') then
            write (L, 'E');
          else
            write (L, 'e');
          end if;
          if (exp < 0) then
            write (L, '-');
          else
            write (L, '+');
          end if;
          exp := abs(exp);
          if (exp < 10) then
            write (L, czero);
          end if;
          write (L, exp);
        else
          -- in "f" format (modified)
          if (exp < 0) then
            write (L, czero);
            dwidth := maximum (dwidth, 4);  -- if exp < -4 or > precision.
            bvalue := bvalue - roundvar;  -- recalculate rounding
            roundvar := half*(10.0**(-dwidth));
            bvalue := bvalue + roundvar;
          else
            write (L, frcptr);  -- integer part (always small)
            bvalue := bvalue - (REAL(frcptr));
            dwidth := dwidth - exp - 1;
          end if;
          if (bvalue > roundvar) then
            writefrc (
              L => L,
              cdes => cdes,
              precision => dwidth,
              value => bvalue);
          end if;
        end if;
      when others => return "";
    end case;
    -- You don't truncate real numbers.
--      if (dot) then                 -- truncate
--        if (L.all'length > fwidth) then
--          return justify (value => L.all (1 to fwidth),
--                          justified => RIGHT,
--                          field => fwidth);         
--        else
--          return justify (value => L.all,
--                          justified => RIGHT,
--                          field => fwidth); 
--        end if;
    if (dash) then  -- fill to fwidth
      return justify (value => L.all,
                      justified => left,
                      field => fwidth);
    else
      return justify (value => L.all,
                      justified => right,
                      field => fwidth);
    end if;
  end function to_string;

end package body standard_additions;
