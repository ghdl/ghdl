library ieee;
use ieee.std_logic_1164.all;
entity reproducer is
    port (
        clk : in std_logic
    );
end entity reproducer;

architecture rtl of reproducer is
    SIGNAL a : std_logic;
begin
    a <= '1' when rising_edge(clk);
end architecture;
