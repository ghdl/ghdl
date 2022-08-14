library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent1 is
	generic (
		WIDTH : positive := 32;
		DEPTH : positive := 256;

		WAYS : positive := 2
	);
	port (
		clk: in std_logic;

		write_enable: in std_logic;
		active_way: in natural range 0 to WAYS-1;
		write_address: in natural range 0 to DEPTH-1;
		input: in std_logic_vector(WIDTH-1 downto 0);

		read_address: in natural range 0 to DEPTH-1;
		outputs: out std_logic_vector(WAYS-1 downto 0)
	);
end entity;

architecture a of ent1 is
begin
	process(clk)
		type memory_t is array(0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);
		type memories_t is array(0 to WAYS-1) of memory_t;

		variable memories : memories_t;
	begin
		if rising_edge(clk) then
			for i in 0 to WAYS-1 loop
				outputs(i) <= and memories(i)(read_address);
			end loop;

			if write_enable then
				memories(active_way)(write_address) := input;
			end if;
		end if;
	end process;
end architecture;
