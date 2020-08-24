library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_last_value_bug is
end entity;

architecture tb of tb_last_value_bug is
    signal cnt : std_logic_vector(3 downto 0) := (others=>'0');
begin
    process
    begin
        wait for 10 ns;
        cnt <= std_logic_vector(unsigned(cnt) + 1);
        report "cnt:  value = " & to_string(cnt) & "  last_value = " & to_string(cnt'last_value) ;
    end process;
end architecture;
