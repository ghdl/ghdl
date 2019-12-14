library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk : in std_logic;
        set : in std_logic;
        reset : in std_logic;
        q : out std_logic
    );
end;

architecture a of ent is
    signal s : std_logic;
begin
    process(clk, set, reset)
    begin
        if set = '1' then
            s <= '1';
        elsif reset = '1' then
            s <= '0';
        elsif rising_edge(clk) then
            s <= not s;
        end if;
    end process;
    q <= s;
end;
