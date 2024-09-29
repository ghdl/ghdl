library ieee;
use ieee.std_logic_1164.all;

entity ibufds is
    generic(
        DIFF_TERM: boolean := true;
        IOSTANDARD: string
    );
    port(
        i: in std_logic;
        ib: in std_logic;
        o: out std_logic
    );
end entity;

architecture sim of ibufds is

begin
    o <= i;
end architecture;
