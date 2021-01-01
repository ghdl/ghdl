library ieee;
use ieee.std_logic_1164.all;

entity bug is
port (
    clock : in std_logic;
    output : out std_logic
);
end bug;

architecture bug_arch OF bug is
begin
    process (clock)
    begin
        if rising_edge(clock) then
            output <= '1';
        end if;
        if rising_edge(clock) then
            output <= '0';
        end if;
    end process;
end bug_arch;
