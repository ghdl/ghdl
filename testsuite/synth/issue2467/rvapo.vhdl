library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package rvapo_ice40_mem_init is
    subtype Line_t is std_logic_vector(255 downto 0);
    type memory_array_t is array (0 to 1) of Line_t;
	type memory_all_t is array (0 to 7) of memory_array_t;

	shared variable MEMORY_INIT_lo:memory_all_t:=(
		0=>(
0=>X"000000000000000000000000000000008F800000000000000000000000000000",
1=>X"000000000000000000000000000000008F800000000000000000000000000000"
			),
		others=>(others=>(others=>'0'))	
		);
end package rvapo_ice40_mem_init;
