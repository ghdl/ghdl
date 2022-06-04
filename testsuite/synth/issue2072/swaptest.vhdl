library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity swaptest is
port (
	clk : in std_logic;
	d : in unsigned(7 downto 0);
	q : out unsigned(7 downto 0)
);
end entity;

architecture rtl of swaptest is

FUNCTION bswap(v : unsigned) RETURN unsigned IS
	VARIABLE u: unsigned(0 TO v'length-1) :=v;
	VARIABLE x: unsigned(0 TO v'length-1);
BEGIN
	FOR i IN 0 TO v'length-1 LOOP
		x((v'length-1)-i):=u(i);
	END LOOP;
	return x;
END FUNCTION;
  
begin

	process(clk) begin 
		if rising_edge(clk) then  	
			q(7 downto 1) <= bswap(d(7 downto 1));
		end if;
	end process;
 
end architecture;

