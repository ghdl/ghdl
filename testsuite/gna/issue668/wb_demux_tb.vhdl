library ieee;
use ieee.std_logic_1164.all;

package wishbone_pkg is
--./wb_demux_tb:internal error: wave.create_type
	type t_wishbone_master_out is record
		dat : std_logic_vector;
	end record;
	subtype t_wishbone_slave_in is t_wishbone_master_out;
end wishbone_pkg;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library work;
use work.wishbone_pkg.all;

entity wb_demux is
    Port (
    	wbs_i	: in t_wishbone_slave_in
    );
end wb_demux;
architecture full_regs of wb_demux is
begin
end full_regs;

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

