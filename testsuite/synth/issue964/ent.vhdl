library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk : in std_logic;
        reset : in std_logic;
        enable : in std_logic;
        q : out std_logic
    );
end;

architecture a of ent is
    signal s : std_logic;
begin
    process(clk, reset)
    begin
        if reset = '1' then
            s <= '0';
        elsif enable /= '1' then
            -- [nothing]
        elsif rising_edge(clk) then
            s <= not s;
        end if;
    end process;

    q <= s;
end;
