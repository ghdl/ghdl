library ieee;
use ieee.std_logic_1164.all;

entity ent is
end;

architecture arch of ent is

    procedure f(a : std_logic_vector(open)) is
    begin
        a(a'high) <= a-1;
    end procedure;

begin
end;
