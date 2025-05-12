library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	generic (
		WDATA     : natural := 8;
		BLOCKS_NB : natural := 2
	);
	port (
		clk        : in  std_logic;
		write_en   : in  std_logic_vector(BLOCKS_NB-1 downto 0);
		write_data : in  std_logic_vector(WDATA-1 downto 0);
		read_data  : out std_logic_vector(BLOCKS_NB*WDATA-1 downto 0)
	);
end top;

architecture synth of top is

	signal reg_blocks : std_logic_vector(BLOCKS_NB*WDATA-1 downto 0) := (others => '0');

begin

	gen_blocks : for h in 0 to BLOCKS_NB-1 generate

		process(clk)
		begin
			if rising_edge(clk) then

				if write_en(h) = '1' then
					reg_blocks((h+1)*WDATA-1 downto h*WDATA) <= write_data;
				end if;

			end if;
		end process;

	end generate;

	read_data <= reg_blocks;

end architecture;
