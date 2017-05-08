entity t4 is
end t4;


library ieee;
use ieee.std_logic_1164.all;

architecture behav of t4 is
  signal s : std_logic;
begin
  b: block
    port (p : out std_logic := '0');
    port map (p => s);
  begin
    process
    begin
      wait for 1 ns;
      p <= '0';
      wait;
    end process;
  end block;

  assert s = '0' severity failure;
end behav;
