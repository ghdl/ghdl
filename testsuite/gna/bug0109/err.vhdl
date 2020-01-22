library ieee;
use ieee.std_logic_1164.all;
use ieee.fixed_pkg.all;


entity phz_calc is
end entity phz_calc;

architecture behavioral of phz_calc is
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
        variable z: sfixed (3 downto -3);
    begin
        z := to_sfixed(3.2,3,-3);
        report "z = " & to_string (z);
        wait;
    end process;
end architecture behavioral;
