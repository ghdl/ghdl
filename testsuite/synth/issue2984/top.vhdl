library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	generic (
		-- Parameters for user memory
		WDATA   : natural := 8;
		WADDR   : natural := 16;
		-- Parameters for internal implementation
		MEMDP   : natural := 256; -- Was 4096
		MEMWD   : natural := 72;
		MEMWA   : natural := 12
	);
	port (
		clk        : in  std_logic;
		-- Write port (in RW configuration, the address is from the first address port)
		write_addr : in  std_logic_vector(WADDR-1 downto 0);
		write_en   : in  std_logic;
		write_data : in  std_logic_vector(WDATA-1 downto 0);
		-- Read ports, inputs
		read_addr  : in  std_logic_vector(WADDR-1 downto 0);
		read_en    : in  std_logic;
		read_data  : out std_logic_vector(WDATA-1 downto 0)
	);
end top;

architecture synth of top is

	type mem_t is array (0 to MEMDP-1) of std_logic_vector(MEMWD-1 downto 0);
	signal mem : mem_t;

	signal reg_read : std_logic_vector(MEMWD-1 downto 0) := (others => '0');

begin

	process(clk)
	begin
		if rising_edge(clk) then

			if read_en = '1' then
				reg_read <= mem(to_integer(unsigned(read_addr(MEMWA-1 downto 0))));
			end if;

			read_data <= std_logic_vector(resize(
				shift_right(unsigned(reg_read), to_integer(unsigned(read_addr(WADDR-1 downto MEMWA))) * WDATA),
				WDATA
			));

		end if;
	end process;

	process(clk)
		variable idx : integer range 0 to MEMWD / WDATA - 1;
	begin
		if rising_edge(clk) then

			idx := to_integer(unsigned(write_addr(WADDR-1 downto MEMWA)));
			if write_en = '1' then
				mem(to_integer(unsigned(write_addr(MEMWA-1 downto 0)))) ((idx+1)*WDATA-1 downto idx*WDATA) <= write_data;
			end if;

		end if;
	end process;

end architecture;

