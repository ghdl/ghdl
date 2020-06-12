library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mwe is
	port(
		n_rst_i		: in std_logic;
		clk_i     	: in std_logic	
	);
end entity mwe;

architecture arch of mwe is

signal counter 					: unsigned(2 DOWNTO 0);

signal weird_clock 				: std_logic;
signal weird_clock_parenthesis 	: std_logic;

begin

process(clk_i)
begin
	
	-- if Reset low then weird_clock = 0
	-- else, weird_clock is set to one for the counter = 1 or 3 or 6

	if n_rst_i = '0' then
		counter <= (others => '0');
		weird_clock <= '0';
		weird_clock_parenthesis <= '0';
	elsif rising_edge(clk_i) then
		counter <= counter+1;
		case (counter) is
			when "001"  => weird_clock <= '1';
			when "011"  => weird_clock <= '1';
			when "110"  => weird_clock <= '1';
			when others => weird_clock <= '0';
		end case;

		case counter is
			when "010"  => weird_clock_parenthesis <= '1';
			when "101"  => weird_clock_parenthesis <= '1';
			when others => weird_clock_parenthesis <= '0';
		end case;
		
	end if;
  end process;
end;
