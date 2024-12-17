library ieee;
use ieee.std_logic_1164.all;
entity attribute_test2 is
end entity attribute_test2;
architecture struct of attribute_test2 is
    signal value : std_logic_vector(7 downto 0);
begin
    process
    begin
        report integer'image(value'left(0));
        wait;
    end process;
end architecture;
