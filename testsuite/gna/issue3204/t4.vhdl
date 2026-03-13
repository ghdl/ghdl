-- file: test.vhd
library ieee;
use ieee.std_logic_1164.all;

package pkg is
  type slv_vector is array (natural range <>) of std_logic_vector;
  function f (x : slv_vector) return natural;
end package;

package body pkg is
  function f (x : slv_vector) return natural is
    variable c : x'element;
  begin
    c := (others => '0');
    return c'length;
  end function;
end package body;
