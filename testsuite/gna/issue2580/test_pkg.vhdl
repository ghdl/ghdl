library ieee;
use ieee.std_logic_1164.all;

package test_pkg is
generic(
    type data_type
);
    type array_data_type is array (natural range <>) of data_type;
end package test_pkg;

