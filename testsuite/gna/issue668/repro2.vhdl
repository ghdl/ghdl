library ieee;
use ieee.std_logic_1164.all;

package wishbone_pkg2 is
  subtype my_vector is std_logic_vector;

	type t_wishbone_master_out is record
		dat : my_vector;
	end record;
	subtype t_wishbone_slave_in is t_wishbone_master_out;
end wishbone_pkg2;

library work;
use work.wishbone_pkg2.all;

library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
end entity;
architecture bench of repro2 is
	signal wbs_s : t_wishbone_slave_in(
		dat(32-1 downto 0)
	);
begin
	stimulus : process
	begin
		wbs_s.dat <= x"deadbeef";
		wait for 100 ns;
		report "pass" severity note;
		wait;
	end process;

	dut : block
          port (wbs_i	: in t_wishbone_slave_in);
          port map (wbs_i => wbs_s);
        begin
        end block;
end architecture;

