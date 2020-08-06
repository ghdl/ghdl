library IEEE;
use IEEE.STD_LOGIC_1164.ALL;	 
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;   

entity bar is
	generic(
		Clk_freq 	: integer := 50;
		Bit_rate 	: integer := 460800
		);
    port (
		rst_i 		: in std_logic;
		clk_i 		: in std_logic;	 
		start_i     : in std_logic;
    	output      : out std_logic_vector(5 downto 0)
  	);
end bar;

architecture bar of bar is

constant Freq_ratio 	: integer := Clk_freq*(10**6)/Bit_rate;
signal period_counter	: std_logic_vector(5 downto 0);
signal res			    : std_logic_vector(5 downto 0);

begin
counter: process(rst_i, clk_i)
begin		
	if rst_i = '1' then
		period_counter <= (others => '0');
	elsif rising_edge(clk_i) then
		if start_i = '1' then 
			if period_counter /= freq_ratio - 1 then
				period_counter <= period_counter + 1;
			else
				period_counter <= (others => '0');
			end if;	
		else
			period_counter <= (others => '0');
		end if;	  
	end if;
end process;	
result: process(rst_i, clk_i)
begin		
	if rst_i = '1' then
		res <= conv_std_logic_vector(0, 6);
	elsif rising_edge(clk_i) then  
		if start_i = '1' then
			res <= period_counter; 							
		else	
			res <= conv_std_logic_vector(0, 6);				
		end if;	
	end if;
end process;	
output <= res;
end bar;
