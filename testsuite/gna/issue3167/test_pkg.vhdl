library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test_pkg is
    generic (W : positive := 8);
    subtype word_t is std_logic_vector(W-1 downto 0);
    function inc(x : word_t) return word_t;
end package;

package body test_pkg is
    function inc(x : word_t) return word_t is
    begin
        return std_logic_vector(unsigned(x) + 1);
    end function;
end;
