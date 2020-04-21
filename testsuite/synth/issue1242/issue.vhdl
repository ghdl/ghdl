library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (bar    :  in std_logic_vector (3 downto 0);
          foobar : out std_logic);
end issue;

architecture beh of issue is
    function foo (bar: std_logic_vector) return std_logic is
        variable i : integer range 0 to 2 := 0;
    begin
        loop
            exit when bar (i) = '0';
            i := i + 1;
        end loop;

        return bar (i);
    end function foo;
begin
    foobar <= foo (bar);
end architecture;
