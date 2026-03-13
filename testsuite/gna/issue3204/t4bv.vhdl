package pkg is
  type bv_vector is array (natural range <>) of bit_vector;
  function f (x : bv_vector) return natural;
end package;

package body pkg is
  function f (x : bv_vector) return natural is
    variable c : x'element;
  begin
    c := (others => '0');
    return c'length;
  end function;
end package body;
