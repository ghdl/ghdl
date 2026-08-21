library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	generic (
		WIDTH : positive := 32;
		DEPTH : positive := 256
	);
	port (
		clk: in std_logic;

		write_enable: in std_logic;
		write_address: in natural range 0 to DEPTH-1;
		input: in std_logic_vector(WIDTH-1 downto 0);

		read_address: in natural range 0 to DEPTH-1;
		output: out std_logic_vector(WIDTH-1 downto 0)
	);
end entity;

architecture a of ent is
begin
	proc: process(clk)
		type memory_t is array(0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);
		variable memory : memory_t;
	begin
		output <= memory(read_address);

		if write_enable = '1' then
			memory(write_address) := input;
		end if;
	end process;
end architecture;
