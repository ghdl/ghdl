entity t5 is
end t5;


library ieee;
use ieee.std_logic_1164.all;

architecture behav of t5 is
  signal s : std_logic := '0';
begin
  b: block
    port (p : out std_logic := 'Z');
    port map (p => s);
  begin
  end block;

  b2: block
    port (p : out std_logic := '1');
    port map (p => s);
  begin
  end block;

  process
  begin
   wait for 1 ns;
   assert s = 'X' severity failure;
   wait;
  end process;
end behav;
