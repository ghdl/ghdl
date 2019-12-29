library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		clk : in std_logic;
		o   : out std_logic_vector(31 downto 0)
	);
end ent;

architecture a of ent is
begin
	process(clk)
		variable var : std_logic_vector(31 downto 0);
	begin
		if rising_edge(clk) then
			var := x"0000_0000";
			o <= x"8000_0000" or var;
		end if;
	end process;
end a;

