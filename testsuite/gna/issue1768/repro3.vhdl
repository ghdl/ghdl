package repro3 is
  constant bv : bit_vector := "1010011";
  function f (vec : bit_vector := bv (0 to 3)) return bit;
end repro3;

package body repro3 is
  function f (vec : bit_vector := bv (0 to 3)) return bit is
  begin
    return '1';
  end f;
end repro3;

