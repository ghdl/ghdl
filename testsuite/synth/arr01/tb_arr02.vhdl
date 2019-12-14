entity tb_arr02 is
end tb_arr02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_arr02 is
  signal v : std_logic_vector(7 downto 0);
  signal h : std_logic_vector(3 downto 0);
  signal l : std_logic_vector(3 downto 0);
begin
  dut: entity work.arr02
    port map (v => v, h => h, l => l);

  process
  begin
    v <= x"a7";
    wait for 1 ns;
    assert h = x"a" severity failure;
    assert l = x"7" severity failure;
    wait;
  end process;
end behav;
