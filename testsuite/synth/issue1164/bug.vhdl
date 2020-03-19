library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		clk                     :  in std_ulogic;
		reset_n                 :  in std_ulogic
	);
end bug;

architecture behav of bug is
	component comp is
	port (		
		data : in std_ulogic_vector
	);
	end component;
	
	type fifo_rdata_t is array (0 to 0) of std_ulogic_vector(4*8-1 downto 0);
	signal fifo_rdata : fifo_rdata_t;
begin
	c : comp
	port map(
		data => fifo_rdata(0)
	);
end architecture;

