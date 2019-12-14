library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk : in std_logic
    );
end;

architecture a of ent is
    procedure inv(signal s : inout std_logic) is
    begin
        s <= not s;
    end procedure;

    signal test : std_logic;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            inv(test);
        end if;
    end process;
end;
