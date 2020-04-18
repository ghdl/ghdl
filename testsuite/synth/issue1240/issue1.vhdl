library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue1 is
    port (foobar : out signed (3 downto 0));
end issue1;

architecture beh of issue1 is
begin
    foobar <= to_signed(natural'(-1), foobar'length);
end architecture;
