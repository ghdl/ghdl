entity tb is
end;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

architecture behav of tb is
  constant cv : slv_vector (1 to 2) (3 downto 0) := (x"5", x"a");
begin
  assert f (cv) = 4 severity failure;
end;
