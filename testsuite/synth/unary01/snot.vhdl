library ieee;
use ieee.std_logic_1164.all;

entity snot is
  generic (
    v : std_logic := '0');
  port (
    o : out std_logic);
end snot;

architecture behav of snot is
begin
  o <= not v;
  assert (not v) = '1' severity failure;
end behav;
