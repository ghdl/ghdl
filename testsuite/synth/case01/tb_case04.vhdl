entity tb_case04 is
end tb_case04;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_case04 is
  signal s : std_logic_vector (4 downto 0);
  signal o : std_logic;
begin
  dut: entity work.case04
    port map (s, o);

  process
  begin
    s <= "00010";
    wait for 1 ns;

    assert o = '1' severity failure;

    wait;
  end process;
end behav;
