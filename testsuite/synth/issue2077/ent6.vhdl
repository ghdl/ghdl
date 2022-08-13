library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent6 is
	generic (
		WIDTH : positive := 8;
		DEPTH : positive := 256
	);
	port (
		clk: in std_logic;

		write_enable: in std_logic;
		write_address: in natural range 0 to DEPTH-1;
		input: in std_logic_vector(WIDTH-1 downto 0);

		read_address: in natural range 0 to DEPTH-1;
		output: out std_logic
	);
end entity;

architecture a of ent6 is
begin
  process(clk)
    type memory_t is array(0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);

    variable memories : memory_t;
  begin
    if rising_edge(clk) then
      output <= and memories(read_address);

      if write_enable then
        memories(write_address) := input;
      end if;
    end if;
  end process;
end architecture;
