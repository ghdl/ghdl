----------------------------------------------------------------------------
-- File: test_pkg.vhd
-- Project:
-- Description: MWE for function recursion
--
--
-- Author: Greg Davey
-- Data modified: 01/26/2019
-- Version: 1.0
----------------------------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.fixed_pkg.all;

package test_pkg is
    type t_sl_array is array(natural range <>) of std_logic;
    type t_slv_array is array(natural range <>) of std_logic_vector;
    type t_sf_array is array (natural range <>) of sfixed;

    type t_complex_sfixed is record
        i : sfixed;
        q : sfixed;
    end record;

    type t_csf_array is array (natural range <>) of t_complex_sfixed;

    type t_cfixed_row_vector is array(positive range <>) of t_complex_sfixed;

    function to_string         (data      : t_complex_sfixed)    return string;

    function to_string         (data      : t_cfixed_row_vector) return string;

    --impure function do_something (samples : integer; ret_type : sfixed) return t_sf_array;

end package;


package body test_pkg is
    --return the string representation of a complex number
    function to_string (
        data : t_complex_sfixed)
        return string is

    begin
        return "(" & real'image(to_real(data.i)) & " + " & real'image(to_real(data.q)) & "i)";
    end function;


    --return the string representation of a complex row vector
    function to_string (
        data : t_cfixed_row_vector)
        return string is
    begin
        if (data'length > 1) then
            return to_string(data(data'low to data'high - 1)) & ", " & to_string(data(data'high));
        else
            return to_string(data(data'low));
        end if;
    end function;


    ----check unconstrained return type
    --impure function do_something (samples : integer; ret_type : sfixed) return t_sf_array is
    --    variable init_array      : t_sf_array(0 to samples - 1)(ret_type'left downto ret_type'right) := (others => (others => '0'));
    --begin
    --    for i in 0 to (samples - 1) loop
    --        init_array(i)   := to_sfixed(1.0 / real(1+i), ret_type);
    --    end loop;
    --    return init_array;
    --end function;
end package body;
