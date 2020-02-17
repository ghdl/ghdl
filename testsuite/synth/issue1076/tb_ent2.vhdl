entity tb_ent2 is
end tb_ent2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent2 is
  signal i   : std_logic;
  signal a   : std_logic;
  signal b   : std_logic;
begin
  dut: entity work.ent2
    port map (i => i, o => a, q => b);

  process
  begin
    
    wait for 1 ns;
    assert a = '0' severity failure;
    wait;
  end process;
end behav;
