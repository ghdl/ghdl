library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	generic (
		WDATA : natural := 8;
		WMEM  : natural := 64  -- Crash if 64, no error if 16 or 8
	);
	port (
		clk        : in  std_logic;
		-- Write port
		write_addr : in  std_logic_vector(15 downto 0);
		write_en   : in  std_logic;
		write_data : in  std_logic_vector(WDATA-1 downto 0);
		-- Read port
		read_addr  : in  std_logic_vector(15 downto 0);
		read_en    : in  std_logic;
		read_data  : out std_logic_vector(WMEM-1 downto 0)
	);
end top;

architecture synth of top is

	type array_of_stdvec is array (natural range <>) of std_logic_vector;
	signal mem : array_of_stdvec(0 to 4096-1)(WMEM-1 downto 0);

begin

	process(clk)
	begin
		if rising_edge(clk) then

			if read_en = '1' then
				read_data <= mem(to_integer(unsigned(read_addr(11 downto 0))));
			end if;

			if write_en = '1' then
				mem(to_integer(unsigned(write_addr(11 downto 0)))) (WDATA-1 downto 0) <= write_data;
			end if;

		end if;
	end process;

end architecture;
