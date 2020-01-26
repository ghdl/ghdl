library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
	port (
		clk : in std_logic;
		i : in std_logic_vector(31 downto 0);
		o : out std_logic_vector(7 downto 0)
	);
end;

architecture a of ent1 is
	function switch_endianness(x : std_logic_vector(31 downto 0)) return std_logic_vector is
	begin
		return x(7 downto 0) & x(15 downto 8) & x(23 downto 16) & x(31 downto 24);
	end function;
begin
	process(clk)
	begin
		if rising_edge(clk) then
			o <= switch_endianness(i)(7 downto 0);
		end if;
	end process;
end;
