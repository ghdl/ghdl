entity tb_case03 is
end tb_case03;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_case03 is
  signal s : std_logic_vector (4 downto 0);
  signal o : std_logic;
begin
  dut: entity work.case03
    port map (s, o);

  process
  begin
    s <= "10011";
    wait for 1 ns;
    assert o = '0' severity failure;

    s <= "00000";
    wait for 1 ns;
    assert o = '0' severity failure;

    wait;
  end process;
end behav;
