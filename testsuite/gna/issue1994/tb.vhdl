library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end entity;

architecture sim of tb is
    signal clk : std_logic := '1';
begin
    clk <= not clk after 100 ms; -- period / 2;
    process is
    begin
        wait until rising_edge(clk);
    end process;
end architecture;
