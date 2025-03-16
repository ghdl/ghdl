library ieee;
use ieee.std_logic_1164.all;

entity reproducer is
    port (
        clk : in std_logic;
        reset : out std_logic
    );
end entity reproducer;

architecture rtl of reproducer is
begin
    reset <= '1'; -- not okay
    -- reset <= '1' WHEN rising_edge(clk); -- okay
end architecture;
