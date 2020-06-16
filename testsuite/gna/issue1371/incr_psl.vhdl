library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- This could be combinational but GHDL does not support @(True) PSL yet
entity incr_psl is
    Port (
        clk: in STD_LOGIC;
        in_val: in UNSIGNED(7 downto 0);
        out_val: out UNSIGNED(7 downto 0)
    );
end entity;

architecture Behavioral of incr_psl is
    signal out_val_internal: UNSIGNED(7 downto 0);
begin
    out_val_internal <= in_val + 1;
    out_val <= out_val_internal;
    -- psl default clock is rising_edge(clk);

    -- The combination of the two below work
    -- psl assert always (in_val = 0 -> out_val_internal = 1);
    -- psl assert always (out_val_internal = 1 -> in_val = 0);
    -- This causes an error
    -- psl assert always (in_val = 0 <-> out_val_internal = 1);
end Behavioral;
