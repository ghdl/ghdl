library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue3 is
    port (foo   : in  integer;
         foobar : out signed (3 downto 0));
end issue3;

architecture beh of issue3 is
    subtype n_int is integer range -1 to 1;
begin
    with n_int'(foo) select
        foobar <= signed'("0001") when -1,
                  signed'("0010") when  0,
                  signed'("0011") when  1;
end architecture;
