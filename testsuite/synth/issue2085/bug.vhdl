library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		clk   : in  std_ulogic;
		src   : in  std_ulogic_vector(15 downto 0);
		dst   : out std_ulogic_vector(16 downto 0)
	);
end bug;

architecture rtl of bug is
begin

process(clk)
	function fun(val : std_ulogic_vector) return std_ulogic_vector is
		variable tmp : val'subtype; --this causes the crash
	begin
		return val;
	end function;
begin
	if rising_edge(clk) then
		dst <= '0' & fun(src);
	end if;
end process;

end architecture;
