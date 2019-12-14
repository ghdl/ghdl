library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sim_types_pkg.all;

entity dma_controller_tb is
generic (
	runner_cfg : string := ""
);
end entity;

architecture bench of dma_controller_tb is

	constant num_desc : positive := 4;
	subtype num_desc_range is natural range 0 to num_desc-1;

	constant MEM_BYTES : positive := 512;
	constant MEM_DWORDS : positive := MEM_BYTES/4;


	function byte_range (desc : descriptor_t) return bit_vector is
		-- Works if 0 to 1
		-- variable v : bit_vector(0 to 1);
		variable v : bit_vector(desc.address to desc.address + desc.length);
	begin
		return v;
	end function;

begin

	mm2s_buffer_destroy: process
		variable desc : descriptor_t := (8, 8);
	begin
		wait for 50 ns;
		for i in byte_range(desc)'range loop
                  call_report(i);
                  assert i >= 8 and i <= 16 severity failure;
		-- Works for below
		--for i in 0 to 1 loop
--			set_permissions(mm2s_memory, i, no_access);
		end loop;
                wait;
	end process;
end architecture;
