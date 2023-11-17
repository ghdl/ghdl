entity tb_ushift01 is
end tb_ushift01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_ushift01 is
  signal v    : unsigned(7 downto 0);
  signal n    : natural;
  signal rao   : unsigned(7 downto 0);
  signal lao   : unsigned(7 downto 0);
  signal rlo   : unsigned(7 downto 0);
  signal llo   : unsigned(7 downto 0);
begin
  dut: entity work.ushift01
    port map (v, n, rlo, llo, rao, lao);

  process
  begin
    v <= x"14";
    for i in 0 to 17 loop
      n <= i;
      wait for 1 ns;
      assert rao = v sra n severity failure;
      assert lao = v sla n severity failure;
      assert rlo = v srl n severity failure;
      assert llo = v sll n severity failure;
    end loop;
    wait;
  end process;
end behav;
