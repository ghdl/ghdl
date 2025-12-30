library ieee;
use ieee.std_logic_1164.all;

entity generic_type_prev_tb is end entity;

architecture formal of generic_type_prev_tb is 
    signal clk_i : std_logic := '1';
    signal data : std_logic := '1';
begin
    clk_i <= not clk_i after 10 ns;
	test_inst : entity work.generic_type_prev generic map( T => std_logic) port map (clk_i => clk_i, data => data); 
end formal;
