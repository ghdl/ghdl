library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reproducer is
end entity reproducer;

architecture rtl of reproducer is
    type std_logic_vector_ptr is access std_logic_vector;
    constant c_header : std_logic_vector(15 downto 0) := x"00" & x"03"; -- produces a bug
    -- constant c_header : std_logic_vector(15 downto 0) := x"0003"; -- size is zero
begin
    process
        variable ptr : std_logic_vector_ptr;
    begin
        report "value unsigned: " & to_string(unsigned(c_header(7 downto 0)));
        report "value integer: " & to_string(to_integer(unsigned(c_header(7 downto 0))));
        ptr := new std_logic_vector(to_integer(unsigned(c_header(7 downto 0))) - 1 downto 0);
        report "size: " & to_string(ptr'length);
        wait for 1 ns;
        deallocate(ptr);
        wait;
    end process;

end architecture;
