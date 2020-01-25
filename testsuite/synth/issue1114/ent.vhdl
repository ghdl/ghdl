library ieee;
use ieee.std_logic_1164.all;

entity ent is
	port (
		clk : in std_logic;
		i : in std_logic_vector(7 downto 0);
		o : out std_logic_vector(3 downto 0)
	);
end;

architecture a of ent is
	function invert(x : std_logic_vector) return std_logic_vector is
	begin
		return not x;
	end function;
begin
	process(clk)
	begin
		if rising_edge(clk) then
			o <= invert(i)(3 downto 0);
		end if;
	end process;
end;
