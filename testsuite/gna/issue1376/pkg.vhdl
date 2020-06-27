package int_arr is
  type int_array_t is array(natural range <>) of integer;
end package;

library ieee;
use ieee.std_logic_1164.all;

package slv_arr is
  type slv_array_t is array(natural range <>) of std_logic_vector;
  type slv_arr_array_t is array(natural range <>) of slv_array_t;
end package ;
