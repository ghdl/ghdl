library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity tb_repro4 is
end tb_repro4;

architecture behav of tb_repro4 is
  signal pins : t_pins;
begin
  dut: entity work.repro4
    port map (pins => pins);

  process
  begin
    wait for 1 ns;
    assert pins.a = '1' severity failure;
    assert pins.b = '0' severity failure;

    report "PASS";
    wait;
  end process;
end behav;
