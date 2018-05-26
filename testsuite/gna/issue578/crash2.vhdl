library ieee;
use ieee.std_logic_1164.all;

package crash_pkg is
    function reorder_vector(inp : std_logic_vector) return std_logic_vector;
end package crash_pkg;

package body crash_pkg is
    function reorder_vector(inp : std_logic_vector) return std_logic_vector is
        variable ret : std_logic_vector(inp'reverse_range);
    begin
        return inp;
        if inp'left < inp'right then
            for i in inp'range loop
                ret(inp'right - i) := inp(i);
            end loop;
        elsif inp'left > inp'right then
            for i in inp'range loop
                ret(inp'left - i) := inp(i);
            end loop;
        else
            ret(inp'left) := inp(inp'left);
        end if;
        return ret;
    end function;
end package body crash_pkg;
