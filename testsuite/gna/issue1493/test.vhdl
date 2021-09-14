library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end test;

architecture test of test is

    subtype word is unsigned (15 downto 0);
    subtype byte is unsigned (7 downto 0);

    signal w: word;
    signal b: byte;
    signal mask: byte;

begin
    w <= (byte'range => (b and mask), others => '1');
end test;
