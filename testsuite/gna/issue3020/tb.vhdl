library ieee;
use ieee.std_logic_1164.all;

use work.gt_dummy_pkg.all;

entity tb is
end;

architecture behav of tb is
  signal e : std_logic := '1';
  signal v : std_logic_vector(3 downto 0) := x"2";
begin
  slv_is_a_part_of_b(e, v);
end;
