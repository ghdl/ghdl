library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity subtype_test is
port
(
    i_Data : in  unsigned; -- (3 downto 0);
    o_Data : out unsigned
);
end entity subtype_test;

architecture RTL of subtype_test is

-- signal Data : unsigned(i_Data'Range) := (others=>'0');
signal Data : i_Data'subtype := (others=>'0');

begin

    Data   <= i_Data;
    o_Data <= Data;

end architecture RTL;
