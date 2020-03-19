library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	generic(
		ADDR_WIDTH   : positive := 32;
		BUS_WIDTH    : positive := 4;
		QUEUE_LENGTH : positive := 32
	);
	port(
		clk          : in std_ulogic;
		reset_n      : in std_ulogic
	);
end bug;

architecture behav of bug is
	signal write_start_addr : unsigned(ADDR_WIDTH-1 downto 0);
	signal num_words        : integer range 0 to QUEUE_LENGTH-1;

	function non_4k_crossing_length(start_addr : unsigned(ADDR_WIDTH-1 downto 0);
	                                max_length : integer range 0 to QUEUE_LENGTH-1) return integer is
		constant words_per_page : integer := 4096/BUS_WIDTH;
		constant diff : integer range 0 to words_per_page := (words_per_page-(to_integer(start_addr)/BUS_WIDTH mod words_per_page));
	begin
		return minimum(diff, max_length);
	end function;

begin

	process(clk, reset_n)
		variable aligned_start_addr : unsigned(ADDR_WIDTH-1 downto 0);
		variable write_length       : integer range 0 to QUEUE_LENGTH-1;
	begin
		if reset_n = '0' then
		elsif rising_edge(clk) then
			aligned_start_addr := resize(write_start_addr/BUS_WIDTH*BUS_WIDTH, ADDR_WIDTH);
			write_length := non_4k_crossing_length(aligned_start_addr, num_words);			
		end if;
	end process;

end architecture;
