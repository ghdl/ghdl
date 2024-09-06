library ieee;
use ieee.std_logic_1164.all;

entity init is
	port (
		top_di : in  std_logic_vector(7 downto 0);
		top_do : out std_logic_vector(9 downto 0)
	);
end init;

architecture synth of init is
	signal sig_di : std_logic_vector(9 downto 0) := (others => '0');
begin

	-- Partial assignment, the rest is supposed to stay to the init value
	-- Issue : This gets extended with high-impedance 'Z'
	sig_di(7 downto 0) <= top_di;

        top_do <= sig_di;
end architecture;
