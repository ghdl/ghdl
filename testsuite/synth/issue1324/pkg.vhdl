library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg is
  type bus_t is record
    data  : std_logic_vector;
    valid : std_logic;
  end record;

end package pkg;
