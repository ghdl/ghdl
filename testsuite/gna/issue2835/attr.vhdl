library ieee;
use ieee.std_logic_1164.all;
entity attribute_test is
end entity attribute_test;
architecture struct of attribute_test is
    type t_std_logic_8_array is array (natural range <>) of std_logic_vector(7 downto 0);
begin
    process
    begin
        report t_std_logic_8_array'base;
        wait;
    end process;
end architecture;
