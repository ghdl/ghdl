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

-- I declare a new type which is an array of buses 
type my_new_type is array(natural range <>) of std_logic_vector(31 downto 0);

-- Then I declare a constant of that new type 
constant constant_of_my_new_type : my_new_type (0 to 7) := ((others=>(others => '0')));

signal signal_of_my_new_type 		: my_new_type (0 to 7);

begin

process(clk_i)
begin
	
	-- if Reset low then signal_of_my_new_type = constant_of_my_new_type
	-- else, signal_of_my_new_type is filled with one at the next clock rising edge

	if n_rst_i = '0' then
		signal_of_my_new_type <= constant_of_my_new_type;
	elsif rising_edge(clk_i) then
		signal_of_my_new_type <= ((others=>(others => '1')));
	end if;
		
end process;

end architecture;
