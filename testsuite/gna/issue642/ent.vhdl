library ieee;
use     ieee.std_logic_1164.all;

entity test is
end entity;

architecture a of test is
	signal Combined : std_logic_vector(16 downto 0);
	
	signal SingleBit : std_logic;
	signal Part1     : std_logic_vector(7 downto 0);
	signal Part2     : std_logic_vector(7 downto 0);
begin
	(Part1, Part2)            <= Combined(Combined'left - 1 downto 0);  -- line 14: can't match 'part1' with type std_ulogic; can't match 'part2' with type std_ulogic
	(SingleBit, Part1, Part2) <= Combined;                              -- line 15: can't match 'part1' with type std_ulogic; can't match 'part2' with type std_ulogic
end architecture;

