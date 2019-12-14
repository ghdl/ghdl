library IEEE;
use     IEEE.std_logic_1164.all;

entity ent is
end entity;

architecture a of ent is
    constant CHECK : natural := 4;
    signal last : std_logic;
    signal clk: std_logic;
begin
    process(clk)
        variable i : natural range 0 to 127 := 0;
    begin
        if rising_edge(clk) then
            if (((i+1) mod CHECK = 0) xor (last = '1')) then
                report "Above line crashes";
            end if;

            i := i + 1;
        end if;
    end process;

end architecture;
