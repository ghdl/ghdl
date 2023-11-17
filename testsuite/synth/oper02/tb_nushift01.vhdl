entity tb_nushift01 is
end tb_nushift01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

architecture behav of tb_nushift01 is
  signal v    : std_logic_vector(7 downto 0);
  signal n    : natural;
  signal rao   : std_logic_vector(7 downto 0);
  signal lao   : std_logic_vector(7 downto 0);
  signal rlo   : std_logic_vector(7 downto 0);
  signal llo   : std_logic_vector(7 downto 0);
begin
  dut: entity work.nushift01
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
