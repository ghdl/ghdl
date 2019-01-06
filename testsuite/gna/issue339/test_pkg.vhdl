library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

package test_pkg is
    type t_sf_array is array (natural range <>) of sfixed;
    impure function do_something(samples : integer; ret_type : sfixed) return t_sf_array;
end package;

package body test_pkg is
    impure function do_something(samples : integer; ret_type : sfixed) return t_sf_array is
        variable init_array      : t_sf_array(0 to samples - 1)(ret_type'left downto ret_type'right) := (others => (others => '0'));

    begin
        for i in 0 to (samples - 1) loop
            init_array(i)   := to_sfixed(1.0/real(1+i), ret_type);
        end loop;
        return init_array;
    end function;

end package body;
