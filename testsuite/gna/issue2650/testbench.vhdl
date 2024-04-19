library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testbench is
end;

architecture arch of testbench is
    type vector_array is array(natural range <>) of std_ulogic_vector;
    signal a : vector_array(0 to 1)(15 downto 0);
begin
    a <= (X"12" & X"34", X"56" & X"78");            -- Should work
    --a <= (0 => X"12" & X"34", 1 => X"56" & X"78");  -- Workaround
end;
