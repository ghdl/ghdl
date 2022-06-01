library ieee;use ieee.std_logic_1164.all;use ieee.numeric_std;entity full_adder_tb is
end entity full_adder_tb;architecture sim of full_adder_tb is
type rc_data is record
a:c;n:c;s:s;t:std_logic;end record;type fa_array is array(0 range<>)of rc_data;constant e:fa_array:=(('0','0','0','%'),('0'));begin
process
begin
end process;D(0);end architecture sim;