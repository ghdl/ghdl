library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity tb_repro5 is
end tb_repro5;

architecture behav of tb_repro5 is
  signal en : std_logic;
  signal pins : t_pins;
  signal rd_a, rd_b : std_logic;
begin
  dut: entity work.repro5
    port map (en => en, pins => pins, rd_a => rd_a, rd_b => rd_b);

  process
  begin
    --  The entity's own drive is tri-stated (en = '0'); the testbench
    --  drives the port from outside.  The entity's own read-back must
    --  see this external value.
    en <= '0';
    pins.a <= '1';
    pins.b <= '0';
    wait for 1 ns;
    assert rd_a = '1' severity failure;
    assert rd_b = '0' severity failure;

    --  The testbench releases the port; the entity's own drive is now
    --  the only one left, and its read-back must see it.
    pins.a <= 'Z';
    pins.b <= 'Z';
    en <= '1';
    wait for 1 ns;
    assert rd_a = '0' severity failure;
    assert rd_b = '1' severity failure;

    report "PASS";
    wait;
  end process;
end behav;
