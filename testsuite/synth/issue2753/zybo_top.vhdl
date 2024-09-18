

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity zybo_top is
  port(
		signal inA : in  std_logic_vector(24 downto 0);
		signal inB : in  std_logic_vector(17 downto 0);
		signal res : out std_logic_vector(31 downto 0)
	);
end zybo_top;

architecture synth of zybo_top is

begin

	-- Simple functionality : This should fit in 1 DSP48E1
	res <= std_logic_vector(resize(signed(inA) * signed(inB), res'length));

end architecture;
