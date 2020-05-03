entity tb_prio02 is
end tb_prio02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_prio02 is
  signal d : std_logic_vector(15 downto 0);
  signal p : natural;
begin
  dut: entity work.prio02
    port map (d, p);

  process
  begin
    d <= x"0004";
    wait for 1 ns;
    assert p = 2 severity failure;

    d <= x"8000";
    wait for 1 ns;
    assert p = 15 severity failure;

    d <= x"0024";
    wait for 1 ns;
    assert p = 5 severity failure;

    wait;
  end process;
end behav;
