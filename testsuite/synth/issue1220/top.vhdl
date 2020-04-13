library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top is
	port(
		clock : in std_logic;
		x : in std_logic_vector(1 downto 0);
		y : in std_logic_vector(1 downto 0);
		data : out std_logic_vector(3 downto 0)
	);
end entity;

architecture arch of top is

	type rom_t is array(0 to 3, 0 to 3) of std_logic_vector(3 downto 0);
	constant rom : rom_t := (
		("0001", "0010", "0100", "1000"),
		("0011", "0110", "1100", "1001"),
		("1110", "1101", "1011", "0111"),
		("0000", "1111", "1111", "0000")
	);
begin
	process (clock)
	begin
		if rising_edge(clock) then
			data <= rom(to_integer(unsigned(x)), to_integer(unsigned(y)));
		end if;
	end process;
end architecture;
