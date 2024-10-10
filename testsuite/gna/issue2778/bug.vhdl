library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bug is end;

architecture arch of bug is
    type vector_array is array(natural range <>) of std_ulogic_vector;
    signal big : vector_array(0 to 3)(7 downto 0);
    signal small : vector_array(0 to 1)(7 downto 0) := (X"00", X"FF");
begin
    big <= (0 to 1 => small, 2 to 3 => small);
end;

