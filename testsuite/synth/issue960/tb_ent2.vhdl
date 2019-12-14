entity tb_ent2 is
end tb_ent2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent2 is
  signal clk : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.ent2
    port map (
      o => dout,
      clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    wait for 1 ns;
    assert dout = '0' severity failure;

    pulse;
    assert dout = '1' severity failure;

    pulse;
    assert dout = '0' severity failure;

    pulse;
    assert dout = '1' severity failure;

    wait;
  end process;
end behav;
