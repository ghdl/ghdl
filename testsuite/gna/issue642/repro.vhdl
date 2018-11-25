entity repro is
end entity;

architecture a of repro is
	signal Combined : bit_vector(16 downto 0);
	
	signal SingleBit : bit;
	signal Part1     : bit_vector(7 downto 0);
	signal Part2     : bit_vector(7 downto 0);
begin
	process
	begin
	  (Part1, Part2)            <= Combined(Combined'left - 1 downto 0);
	  (SingleBit, Part1, Part2) <= Combined;
          wait;
        end process;
end architecture;

