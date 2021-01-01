library ieee;
use ieee.std_logic_1164.all;

entity bug4 is
port (
    clock : in std_logic;
    output : out std_logic
);
end;

architecture bug_arch OF bug4 is
  signal t : std_logic := '1';
begin
    process (clock)
    begin
        if rising_edge(clock) then
            t <= '1';
        end if;
        if rising_edge(clock) then
            t <= '0';
        end if;
    end process;

    output <= t;
end bug_arch;
