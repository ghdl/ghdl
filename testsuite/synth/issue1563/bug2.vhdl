library ieee;
use ieee.std_logic_1164.all;

entity bug2 is
port (
    clock : in std_logic;
    output : out std_logic_vector(3 downto 0)
);
end;

architecture bug_arch OF bug2 is
begin
    process (clock)
    begin
        if rising_edge(clock) then
            output <= "0010";
        end if;
        if rising_edge(clock) then
            output(2) <= '1';
        end if;
    end process;
end bug_arch;
