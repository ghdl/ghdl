library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_pkg.all;

entity wb_demux_tb is
end entity;

architecture bench of wb_demux_tb is

	signal wbs_i : t_wishbone_slave_in(
		dat(32-1 downto 0)
	);

begin

	stimulus : process
	begin
		wbs_i.dat <= x"deadbeef";
		wait for 100 ns;
		report "pass" severity note;
		wait;
	end process;

	dut : entity work.wb_demux
	port map (
		wbs_i => wbs_i
	);


end architecture;
