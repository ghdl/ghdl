library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		clk : in std_logic;
		i : in std_logic_vector(7 downto 0);
		o : out std_logic_vector(7 downto 0)
	);
end;

architecture a of ent is
	signal a : std_logic_vector(7 downto 0) := x"42";
	signal b : std_logic_vector(7 downto 0) := a;
begin
	process(clk)
	begin
		if rising_edge(clk) then
			a <= i;
			b <= a;
		end if;
	end process;

	o <= b;
end;
