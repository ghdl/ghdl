library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent5 is
	generic (
		WIDTH : positive := 8;
		DEPTH : positive := 256;

		WAYS : positive := 4
	);
	port (
		clk: in std_logic;

		write_enable: in std_logic;
		active_way: in natural range 0 to WAYS-1;
		write_address: in natural range 0 to DEPTH-1;
		input: in std_logic_vector(WIDTH-1 downto 0);

		read_address: in natural range 0 to DEPTH-1;
		output: out std_logic_vector(WIDTH-1 downto 0)
	);
end entity;

architecture a of ent5 is
begin
  process(clk)
    type memory_t is array(0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);
    type memories_t is array(0 to WAYS-1) of memory_t;

    variable memories : memories_t;
  begin
    if rising_edge(clk) then
      output <= memories(active_way)(read_address);

      if write_enable then
        memories(active_way)(write_address) := input;
      end if;
    end if;
  end process;
end architecture;
