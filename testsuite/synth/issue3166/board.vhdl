library ieee;
use ieee.std_logic_1164.all;

entity board is
    port(
        btn1 : in std_logic;
        btn2 : in std_logic;
        led: out std_logic
    );
end entity;

architecture behavior of board is
    signal tmp: std_logic := '0';
begin
    -- both buttons and leds are active low
    process(btn1, btn2)
    begin
        if rising_edge(btn1) then
            tmp <= '1';
        elsif rising_edge(btn2) then
            tmp <= '0';
        end if;

        led <= tmp;
    end process;
end architecture;
