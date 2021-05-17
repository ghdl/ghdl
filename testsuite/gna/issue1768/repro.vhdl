package pkg is
  function log2 (v : positive) return natural;
end pkg;

package body pkg is
  function log2 (v : positive) return natural is
  begin
    return 2;
  end log2;
end pkg;

use work.pkg.all;

package repro is
  constant W : natural := 8;
  function f (vec : bit_vector(log2(W / 2) - 1 downto 0)) return bit;
end repro;

package body repro is
  function f (vec : bit_vector(log2(W / 2) - 1 downto 0)) return bit is
  begin
    return '1';
  end f;
end repro;

