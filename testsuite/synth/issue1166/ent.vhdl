library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        a      : inout std_logic;
        enable : in std_logic;
        d_in   : in std_logic;
        d_out  : out std_logic
    );
end;

architecture a of ent is
begin
    process(all)
    begin
        if enable then
            a <= d_in;
        else
            a <= 'Z';
        end if;
    end process;
    d_out <= a;
end;
