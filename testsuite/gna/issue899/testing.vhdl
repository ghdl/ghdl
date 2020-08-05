library IEEE;
use IEEE.NUMERIC_STD.ALL;

entity testing is
generic(
    GENVAL : integer := 4);
end entity;

architecture RTL of testing is
    type pixel_line is array(natural range <>) of bit_vector;
    signal data_in : pixel_line(0 to GENVAL)(7 downto 0);
begin
end architecture;
