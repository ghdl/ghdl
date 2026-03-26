package pkg_a is
  type idx_t is (x, y, z);
  type arr_t is array (idx_t) of bit_vector(7 downto 0);
end package;

package pkg_b is
  type idx_t is (x, y, z);
  type arr_t is array (idx_t) of bit_vector(7 downto 0);
end package;

use work.pkg_a.all;

entity tb is end;

architecture sim of tb is
  signal a : arr_t := (others => x"00");
  signal b : work.pkg_b.arr_t;
begin
  b <= work.pkg_b.arr_t(a);   -- THIS LINE CRASHES GHDL
end;
