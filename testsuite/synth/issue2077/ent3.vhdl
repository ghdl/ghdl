library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent3 is
	generic (
		DEPTH : positive := 256;
		WAYS : positive := 4
	);
	port (
		clk: in std_logic;

		write_enable: in std_logic;
		active_way: in natural range 0 to WAYS-1;
		write_address: in natural range 0 to DEPTH-1;
		input: in std_logic;

		read_address: in natural range 0 to DEPTH-1;
		outputs: out std_logic
	);
end entity;

architecture a of ent3 is
begin
	process(clk)
--          type memory_t is array(0 to DEPTH-1) of std_logic;
          type memories_t is array(0 to WAYS-1, 0 to DEPTH-1) of std_logic;

          variable memories : memories_t;
	begin
          if rising_edge(clk) then
            outputs <= memories(active_way, read_address);

            if write_enable then
              memories(active_way, write_address) := input;
            end if;
          end if;
	end process;
end architecture;
