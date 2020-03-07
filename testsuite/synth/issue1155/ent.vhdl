library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	port (
		clk   : in std_logic;
		write : in std_logic;

		addr       : in std_logic_vector(1 downto 0);
		data_write : in std_logic_vector(3 downto 0);

		x0 : out std_logic_vector(3 downto 0);
		x1 : out std_logic_vector(3 downto 0);
		x2 : out std_logic_vector(3 downto 0);
		x3 : out std_logic_vector(3 downto 0)
	);
end;

architecture a of ent is
	type store_t is array(0 to 3) of std_logic_vector(3 downto 0);
	signal store : store_t;
begin
	process(clk)
	begin
		if rising_edge(clk) and write = '1' then
			store(to_integer(unsigned(addr))) <= data_write;
		end if;
	end process;

	x0 <= store(0);
	x1 <= store(1);
	x2 <= store(2);
	x3 <= store(3);
end;
