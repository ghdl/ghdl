library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ghdl_bug is
end ghdl_bug;
architecture ghdl_bug_arch of ghdl_bug is

function fail_msg_data(
	data 	: std_logic_vector
)	return string is
	variable data_nat : natural 
                   := to_integer(unsigned(data(min(28, 24) downto 0)));
begin
	return "data=" &  integer'image(data_nat);
end function;

begin
end ghdl_bug_arch;
