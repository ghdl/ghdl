library ieee;
use ieee.std_logic_1164.all;

entity e is
end entity e;

architecture a of e is

signal operator_for_cmp : std_logic_vector(7 downto 0) := (others => 'X');

begin

process (operator_for_cmp) is
begin
   case operator_for_cmp is
	when "00000000" =>
		null;
	when "00000001" =>
		null;
	when "00000002" => -- Me being stupid
		null;
	when "00000003" => --Again
      when others => null;
   end case;
end process;

end architecture;
