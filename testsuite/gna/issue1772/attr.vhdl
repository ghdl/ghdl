package attr_pkg is
  type string_array is array (natural range <>) of string;
  type pin_bit_info is array (natural range <>) of string_array;

  attribute pin_bit_information : pin_bit_info;

  function tconv (a : bit) return bit;

  attribute pin_bit_information of tconv : function is
    (1 => ("   ", "A0      ", "INCR", "PIN_BIT_SCALAR"),
     2 => ("   ", "10      ", "INCR", "PIN_BIT_SCALAR"));
end package;

package body attr_pkg is
  function tconv (a : bit) return bit is
  begin
    return not a;
  end tconv;
end package body;
