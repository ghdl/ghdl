library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity entity_1 is
	generic (
		FREQ : real     := 100.0;
		BITS : positive := 8
	);
	port (
	  Clock: in  std_logic;
	  Reset: in  std_logic := '0';
	  Q:     out std_logic_vector(BITS - 1 downto 0)
	);
end entity entity_1;

architecture behav of entity_1 is
begin
	process(Clock)
	begin
		if rising_edge(Clock) then
			if Reset = '1' then
				Q <= (others => '0');
			else
				Q <= std_logic_vector(unsigned(Q) + 1);
			end if;
		end if;
	end process;
end architecture behav;
