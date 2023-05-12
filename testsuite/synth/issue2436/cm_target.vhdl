library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
use WORK.rvapo_cm_target_init.all;

entity CombinedMemory_Target is
	port (
		clock: in Clock_t;
		addr_1: in std_logic_vector (31 downto 0);
		write_1: in std_logic_vector (31 downto 0);
		web: in std_logic_vector(3 downto 0)
	);
end;

architecture rtl of CombinedMemory_Target is
	shared variable memory: memory_array_t := (others => x"00");
	subtype Index_t is unsigned(31 downto 0);
begin
	port_1_write: process (clock, addr_1, write_1)
		variable index: Index_t;
	begin
--		if rising_edge(clock.pulse) then
			if clock.reset /= '1' and clock.enable = '1' and rising_edge(clock.pulse) then
				index := unsigned(addr_1);

				if web(0)='1' then memory(to_integer(index)):=write_1(7 downto 0);end if;
				if web(1)='1' then memory(to_integer(index)+1):=write_1(15 downto 8);end if;
			end if;
--		end if;
	end process;
end;
