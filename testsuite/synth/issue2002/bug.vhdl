library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		o : out std_ulogic_vector(33 downto 0)
	);
end bug;

architecture struct of bug is
    constant c : std_ulogic_vector(33 downto 0) := "0000000000000000000000000001111100";
begin
    o <= c;
end architecture;
