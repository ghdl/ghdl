entity tb_view02 is
end tb_view02;

library ieee;
use ieee.std_logic_1164.all;
use work.view02_pkg.all;

architecture behav of tb_view02 is
  signal inp_ctrl   : std_logic;
  signal inp_data_a : std_logic;
  signal inp_data_b : std_logic;
  signal bus_s      : outer_t;
begin
  dut: entity work.view02
    port map (
      inp_ctrl   => inp_ctrl,
      inp_data_a => inp_data_a,
      inp_data_b => inp_data_b,
      m          => bus_s);

  process
  begin
    inp_ctrl   <= '1';
    inp_data_a <= '0';
    inp_data_b <= '1';
    wait for 1 ns;
    assert bus_s.ctrl   = '1' severity failure;
    assert bus_s.data.a = '0' severity failure;
    assert bus_s.data.b = '1' severity failure;

    inp_ctrl   <= '0';
    inp_data_a <= '1';
    inp_data_b <= '0';
    wait for 1 ns;
    assert bus_s.ctrl   = '0' severity failure;
    assert bus_s.data.a = '1' severity failure;
    assert bus_s.data.b = '0' severity failure;

    wait;
  end process;
end behav;
