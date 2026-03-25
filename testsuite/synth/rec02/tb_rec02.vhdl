entity tb_rec02 is
end tb_rec02;

library ieee;
use ieee.std_logic_1164.all;
use work.rec02_pkg.all;

architecture behav of tb_rec02 is
  signal p  : outer_t;
  signal oa : std_logic;
  signal ob : std_logic;
  signal oz : std_logic;
begin
  dut: entity work.rec02
    port map (p => p, oa => oa, ob => ob, oz => oz);

  process
  begin
    p <= (i => (a => '1', b => '0'), z => '1');
    wait for 1 ns;
    assert oa = '1' severity failure;
    assert ob = '0' severity failure;
    assert oz = '1' severity failure;

    p <= (i => (a => '0', b => '1'), z => '0');
    wait for 1 ns;
    assert oa = '0' severity failure;
    assert ob = '1' severity failure;
    assert oz = '0' severity failure;

    wait;
  end process;
end behav;
