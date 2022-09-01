library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.float_pkg.all;

entity to01_float is 
port(
i0 : in  float (7 downto -6);
p0 : out float (7 downto -6));
end to01_float ;

architecture arch1 of to01_float is 

begin
--  p0 <= to_01(i0);
  p0 <= UNRESOLVED_float (STD_LOGIC_VECTOR(to_01(UNSIGNED(to_sulv(i0)))));
end arch1;
