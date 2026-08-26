library ieee;
use ieee.std_logic_1164.all;

entity other_thing is
end entity;

architecture structural of other_thing is
    component something is
        port(
            clock: out std_logic
        );
    end component;

    signal clock: std_logic;
begin
    something_instance: something port map(clock);
end architecture;
