library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity tb_repro1 is
end tb_repro1;

architecture behav of tb_repro1 is
  signal en : std_logic := '0';
  signal pins : t_pins;
begin
  dut: entity work.repro1
    port map (en => en, pins => pins);

  process
  begin
    en <= '0';
    wait for 1 ns;
    assert pins.a = 'Z' severity failure;
    assert pins.b = 'Z' severity failure;

    en <= '1';
    wait for 1 ns;
    assert pins.a = '1' severity failure;
    assert pins.b = '0' severity failure;

    en <= '0';
    wait for 1 ns;
    assert pins.a = 'Z' severity failure;
    assert pins.b = 'Z' severity failure;

    wait;
  end process;
end behav;
