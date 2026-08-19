library ieee;
use ieee.std_logic_1164.all;

entity circuit is
    port (
        clk : std_logic;
        sig : out std_logic
    );
end entity;

architecture rtl of circuit is
begin
    process (clk)
    begin
        if falling_edge(clk) then
            -- assertion failure only as long as this branch exists
        elsif rising_edge(clk) then
            -- assertion failure only when assignment is here
            sig <= '1';
        end if;
    end process;
end architecture;
