entity test_ghdl is
    port (
        clk_o : out std_logic
    );
end entity test_ghdl;
library ieee;
use ieee.std_logic_1164.all;
architecture struct of test_ghdl is
    signal clk : std_logic := '0';
begin
    clk <= not clk after 10 ns;
    clk_o <= clk;
end architecture;
