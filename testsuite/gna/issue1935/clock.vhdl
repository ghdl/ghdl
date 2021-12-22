library IEEE;
use IEEE.std_logic_1164.all;

entity clock is
	port(
		interval: in time;
		EN      : in std_logic;
		CLK     : out std_logic
	);
	
end clock;

architecture behave of clock is

begin	
	
	clock : process
	begin
		loop
			exit when EN = '0';
			CLK <= not '1';
			wait for interval;
			CLK <= not '0';
			wait for interval;
		end loop;
		
		CLK <= '0';
		wait until EN = '1';
	end process;
end architecture;
