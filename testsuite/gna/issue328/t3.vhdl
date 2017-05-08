entity t3 is
end t3;


library ieee;
use ieee.std_logic_1164.all;

architecture behav of t3 is
  signal s : std_logic;
begin
  b: block
    port (p : out std_logic := '0');
    port map (p => s);
  begin
  end block;

  assert s = '0' severity failure;
end behav;
