library ieee;use ieee.std_logic_1164.all;use ieee.numeric_std.all;entity full_adder_tb is
end entity full_adder_tb;architecture m of full_adder_tb is--
type rc_data is record a:c;c:std_logic;end record rc_data;type fa_array is array(0)of rc_data;constant f:fa_array:=(('0'),('0','%'));begin process begin
for i in 0 loop
end loop;end process;r(0);end architecture;