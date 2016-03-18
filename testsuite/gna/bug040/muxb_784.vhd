library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity muxb_784 is
	port (
		in_sel : in  std_logic;
		out_data : out std_logic_vector(31 downto 0);
		in_data0 : in  std_logic_vector(31 downto 0);
		in_data1 : in  std_logic_vector(31 downto 0)
	);
end muxb_784;

architecture augh of muxb_784 is
begin

	out_data <= in_data0 when in_sel = '0' else in_data1;

end architecture;

