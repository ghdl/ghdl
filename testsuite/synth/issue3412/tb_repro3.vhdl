library ieee;
use ieee.std_logic_1164.all;

entity tb_repro3 is
end tb_repro3;

architecture behav of tb_repro3 is
  signal en : std_logic := '0';
  signal pin_a, pin_b : std_logic;
begin
  dut: entity work.repro3
    port map (en => en, pin_a => pin_a, pin_b => pin_b);

  process
  begin
    en <= '0';
    wait for 1 ns;
    assert pin_a = 'Z' severity failure;
    assert pin_b = 'Z' severity failure;

    en <= '1';
    wait for 1 ns;
    assert pin_a = '1' severity failure;
    assert pin_b = '0' severity failure;

    en <= '0';
    wait for 1 ns;
    assert pin_a = 'Z' severity failure;
    assert pin_b = 'Z' severity failure;

    wait;
  end process;
end behav;
