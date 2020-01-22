library ieee;
use ieee.std_logic_1164.all;

package foo_pkg is
    type unresolved_sfixed is array (integer range <>) of std_ulogic;
    subtype sfixed is (resolved) UNRESOLVED_sfixed;
    function to_string (inp: unresolved_sfixed) return string;
end package foo_pkg;

package body foo_pkg is
    function to_string (inp: unresolved_sfixed) return string is
        variable image_str: string (1 to inp'length + 1);
        variable j: integer range 1 to image_str'length + 1;
    begin
        j := 1;
        for i in inp'range loop
            
            if i = -1 then
                image_str(j) := ',';
                j := j + 1;
            end if;
            image_str(j) := character'VALUE(std_ulogic'IMAGE(inp(i)));
            j := j + 1;
        end loop;
        return image_str;
    end function;
end package body foo_pkg;

library ieee;
use ieee.std_logic_1164.all;
use work.foo_pkg.all;

entity foo is
end entity;

architecture fum of foo is
    constant sfixed_val:    sfixed (3 downto -4):= x"da";
    function to_string (inp: sfixed) return string is
        variable image_str: string (1 to inp'length + 1);
        variable j: integer range 1 to image_str'length + 1;
    begin
        j := 1;
        for i in inp'range loop
            
            if i = -1 then
                image_str(j) := '.';
                j := j + 1;
            end if;
            image_str(j) := character'VALUE(std_ulogic'IMAGE(inp(i)));
            j := j + 1;
        end loop;
        return image_str;
    end function;
begin
    process
    begin
        report "sfixed_val = " & to_string(sfixed_val);
        wait;
    end process;
end architecture;
