library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue2 is
    port (foobar : out signed (3 downto 0));
end issue2;

architecture beh of issue2 is
    subtype my_type is natural range 0 to 1; -- width = 1
begin
    foobar <= to_signed(my_type'(-15), foobar'length);
end architecture;
