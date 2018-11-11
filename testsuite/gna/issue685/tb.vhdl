library ieee;
use ieee.std_logic_1164.all;

package wishbone_pkg is
	type t_wishbone_master_out is record
		dat : std_logic_vector;
		-- Works properly when field we is declared before dat
		we  : std_logic;
	end record;
end wishbone_pkg;

library ieee;
use ieee.std_logic_1164.all;
use work.wishbone_pkg.all;

entity wb_demux_tb is
end entity;
architecture bench of wb_demux_tb is
	signal s : t_wishbone_master_out(
		dat(1 downto 0)
	);
begin
	stimulus : process
	begin
		wait for 1 ns;
		s.dat <= "11";
		wait for 1 ns;
		s.dat <= "00";
		wait for 1 ns;
		report "pass";
		std.env.finish;
	end process;
end architecture;
