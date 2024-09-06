library ieee;
use ieee.std_logic_1164.all;

entity initport is
	port (
		top_di : in  std_logic_vector(7 downto 0);
		top_do : out std_logic_vector(9 downto 0) := (others => '0')
	);
end;

architecture synth of initport is
begin
  top_do(7 downto 0) <= top_di;
end architecture;
