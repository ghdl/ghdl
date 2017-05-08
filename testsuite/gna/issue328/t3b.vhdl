entity t3b is
end t3b;


library ieee;
use ieee.std_logic_1164.all;

architecture behav of t3b is
  signal s : std_logic := '0';
begin
  b: block
    port (p : out std_logic);
    port map (p => s);
  begin
  end block;

  process
  begin
   wait for 1 ns;
   assert s = 'U' severity failure;
   wait;
  end process;
end behav;
