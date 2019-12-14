library ieee;
use ieee.std_logic_1164.all;

entity lut is port (
    sel: in std_logic_vector (1 downto 0);
    c: out std_logic);
end lut;

-- sel(1)  sel(0) |  c 
--   0       0    |  1    
--   0       1    |  0
--   1       0    |  1
--   1       1    |  0

architecture synth of lut is
begin

with sel select c <=

    '1' when "00",
    '0' when "01",
    '1' when "10",
    '0' when others;

end synth;
