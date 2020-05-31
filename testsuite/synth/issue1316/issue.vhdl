library ieee;
use ieee.std_logic_1164.all;

entity issue is
end issue;

architecture beh of issue is

    procedure foo is
        variable cnt : integer;
    begin
        cnt := cnt - 1;
    end procedure;

begin
    foo;
end architecture beh;
