library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cover_overlap_misparse is
    Port (
        clk: in STD_LOGIC;
        counter_val: out STD_LOGIC_VECTOR(3 downto 0)
    );
end entity;

architecture Behavioral of cover_overlap_misparse is
    signal ctr_internal: UNSIGNED(3 downto 0) := x"0";
begin
    process(clk) is
    begin
        ctr_internal <= ctr_internal + 1;
    end process;
    counter_val <= STD_LOGIC_VECTOR(ctr_internal);

    -- psl default clock is rising_edge(clk);
    --psl assert always ctr_internal = 0 |-> ctr_internal = 0;
end Behavioral;
