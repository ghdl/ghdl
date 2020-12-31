library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity e1 is
	generic (
		BITS : positive := 8
	);
	port (
	  Clock: in  std_logic;
	  Reset: in  std_logic;
	  Q:     out std_logic_vector(BITS - 1 downto 0)
	);
end entity e1;

architecture behav of e1 is
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
