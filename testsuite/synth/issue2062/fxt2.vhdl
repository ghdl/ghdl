library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity fxt2 is port (
   a : in  std_logic_vector(5 downto 0);
   y : out ufixed(3 downto -2));
end entity;

architecture beh of fxt2 is
begin
   y <= to_ufixed(a, 6, 1);
end beh;
