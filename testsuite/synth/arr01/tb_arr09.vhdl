entity tb_arr09 is
end tb_arr09;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_arr09 is
  signal val : std_logic_vector(3 downto 0);
  signal res : character;
begin
  dut: entity work.arr09
    port map (val => val, res => res);

  process
  begin
    wait for 1 ns;
    assert res = '3' severity failure;

    wait;
  end process;
end behav;
