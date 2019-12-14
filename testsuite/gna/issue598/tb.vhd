library ieee;
use ieee.std_logic_1164.all;

entity tb is
end entity;

architecture tb of tb is
begin
    process
        constant a : std_logic_vector(3 downto 0) := x"A";
        variable b : std_logic_vector(3 downto 0) := x"B";
    begin
        report to_string(b); -- OK
        report to_string(a); -- fails
	assert to_string(a) = "1010";
	assert to_string(b) = "1011";
	wait;
    end process;
end architecture;

