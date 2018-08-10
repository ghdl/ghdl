library ieee;
use ieee.std_logic_1164.all;

package wishbone_pkg is

--./wb_demux_tb:internal error: waves.write_types: unhandled obj kind
type t_wishbone_slave_in is record
	dat : std_logic_vector;
end record;


--./wb_demux_tb:internal error: wave.create_type
--type t_wishbone_master_out is record
--	dat : std_logic_vector;
--end record;
--subtype t_wishbone_slave_in is t_wishbone_master_out;

end wishbone_pkg;
