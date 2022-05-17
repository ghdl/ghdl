library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.fixed_pkg.all;

entity fxt is port (
   a : in  std_logic_vector(6 downto 0);
   y : out ufixed(3 downto -2));
end entity;

architecture beh of fxt is
begin
   y <= to_ufixed(a, 5, 1);
end beh;
