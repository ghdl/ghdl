

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	generic (
		WDATA : natural := 32;
		CELLS : natural := 64;
		WADDR : natural := 6
	);
	port (
		clk    : in  std_logic;
		wren   : in  std_logic;
		wraddr : in  std_logic_vector(WADDR-1 downto 0);
		wrdata : in  std_logic_vector(WDATA-1 downto 0);
		rdaddr : in  std_logic_vector(WADDR-1 downto 0);
		rddata : out std_logic_vector(WDATA-1 downto 0)
	);
end top;

architecture synth of top is

	type mem_type is array(0 to CELLS-1) of std_logic_vector(WDATA-1 downto 0);
	signal mem : mem_type;

	attribute ram_style : string;
	attribute ram_style of mem : signal is "distributed";

begin

	process(clk)
	begin
		if rising_edge(clk) then
			if wren = '1' then
				mem(to_integer(unsigned(wraddr))) <= wrdata;
			end if;
		end if;
	end process;

	rddata <= mem(to_integer(unsigned(rdaddr)));

end architecture;
