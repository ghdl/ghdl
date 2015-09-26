use std.textio.all;

package pkg is
  type enum_t is (a,b,c,d);
  type arr_t is array (enum_t range <>) of line;
  shared variable arr : arr_t(a to d) := (others => null);
end package;

package body pkg is
end package body pkg;
