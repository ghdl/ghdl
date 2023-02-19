library ieee;
use ieee.std_logic_1164.all;

package a is
    generic (
        G_a: integer
    );

    type slv8_array_t is array (natural range <>) of std_logic_vector(7 downto 0);

    type my_type_t is record
        a   : slv8_array_t(0 to G_a - 1);
    end record my_type_t;

end package a;
