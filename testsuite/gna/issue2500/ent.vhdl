library ieee;
use     ieee.std_logic_1164.all;

entity ent is
    generic(
        REG_COUNT: natural
    );
    port(
        clk: in std_logic;
        rst: in std_logic;

        i_valid: in std_logic;
        o_valid: out std_logic
    );
end entity;

architecture arch of ent is
begin
    -- psl default clock is rising_edge(clk);
    -- psl assume {rst};

    -- psl f_delay_valid: assert always
    --   {i_valid} |-> {[*0 to REG_COUNT]; o_valid}
    --   sync_abort rst;
end architecture;
