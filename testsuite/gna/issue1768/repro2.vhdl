package pkg2 is
  type natural_array is array (positive range <>) of natural;
  constant log2 : natural_array := (1 => 0,
                                    2 => 1,
                                    3 to 4 => 2,
                                    5 to 8 => 3);
end pkg2;

use work.pkg2.all;

package repro2 is
  constant W : natural := 8;
  function f (vec : bit_vector(log2(W / 2) - 1 downto 0)) return bit;
end repro2;

package body repro2 is
  function f (vec : bit_vector(log2(W / 2) - 1 downto 0)) return bit is
  begin
    return '1';
  end f;
end repro2;

