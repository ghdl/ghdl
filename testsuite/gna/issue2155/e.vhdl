library ieee;
use ieee.std_logic_1164.all;

use work.closely_related_arrays.all;

entity e is
end;

architecture behav of e is
begin
  assert sv (1) = std_logic_vector (uv (1)) severity failure;
end behav;
