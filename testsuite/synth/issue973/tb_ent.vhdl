entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal i : std_logic_vector (7 downto 0);
  signal o : std_logic_vector (3 downto 0);
begin
  dut: entity work.ent
    port map (i, o);

  process
  begin
    i <= x"b6";
    wait for 1 ns;
    assert o = x"b" severity failure;

    wait;
  end process;
end behav;
