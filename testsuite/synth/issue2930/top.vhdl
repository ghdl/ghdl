library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	generic (
		DATA_BITS : natural := 32;
		CELLS     : natural := 2**20; --  Was: 2 ** 24;
		ADDR_BITS : natural := 32
	);
	port (
		clk   : in  std_logic;
		wen   : in  std_logic;
		waddr : in  std_logic_vector(ADDR_BITS-1 downto 0);
		wdata : in  std_logic_vector(DATA_BITS-1 downto 0);
		ren   : in  std_logic;
		raddr : in  std_logic_vector(ADDR_BITS-1 downto 0);
		rdata : out std_logic_vector(DATA_BITS-1 downto 0)
	);
end top;

architecture synth of top is

	type mem_type is array(0 to CELLS-1) of std_logic_vector(DATA_BITS-1 downto 0);
	--signal mem : mem_type;
	signal mem : mem_type := (others => (others => '0'));

	attribute ram_style : string;
	attribute ram_style of mem : signal is "block";

begin

	process(clk)
	begin
		if rising_edge(clk) then

			if ren = '1' then
				rdata <= mem(to_integer(unsigned(raddr)));
			end if;

			if wen = '1' then
				mem(to_integer(unsigned(waddr))) <= wdata;
			end if;

		end if;
	end process;

end architecture;
