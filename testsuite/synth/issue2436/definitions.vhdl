library IEEE;
use IEEE.std_logic_1164.all;
package rvapo_cm_target_init is
type Clock_t is record
	pulse: std_logic;
	enable: std_logic;
	reset: std_logic;
end record;

subtype Byte_t is std_logic_vector(7 downto 0);
type memory_array_t is array (0 to 65535 + 4) of Byte_t;
end package rvapo_cm_target_init;
