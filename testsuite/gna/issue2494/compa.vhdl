library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.compa_pkg.all;

entity compa is
	port (
		clk		:in  	std_logic;
		rst		:in  	std_logic;
		siga 	:inout 	std_logic_vector(7 downto 0); --Mismatch on this is detectected
		sigb	:inout 	test_rec(a(11 downto 0),b(11 downto 0))
	);
end entity;

architecture behv of compa is
begin

process (clk)
begin
	if rising_edge(clk) then
		if rst then
			siga 	<= (others => '0');
			sigb.a	<= 12x"123";
		else
			siga <= (others => '1');
			sigb.a 	<= 12x"456";

		end if;
	end if;
end process;

end architecture;
