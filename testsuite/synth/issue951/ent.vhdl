library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk : in std_logic;
        enable : in std_logic;
        i : in std_logic;
        o : out std_logic
    );
end;

architecture a of ent is
begin
    process(clk)
    begin
        -- works:
        --if rising_edge(clk) and enable = '1' then
        if enable = '1' and rising_edge(clk) then
            o <= i;
        end if;
    end process;
end;
