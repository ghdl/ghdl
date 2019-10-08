entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal clk : std_logic;
  signal dout : std_logic;
  signal enable : std_logic;
  signal reset : std_logic;
begin
  dut: entity work.ent
    port map (
      enable => enable,
      reset => reset,
      q => dout,
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
    enable <= '1';
    reset <= '1';
    pulse;
    assert dout = '0' severity failure;

    enable <= '1';
    reset <= '0';
    pulse;
    assert dout = '1' severity failure;

    pulse;
    assert dout = '0' severity failure;

    pulse;
    assert dout = '1' severity failure;

    enable <= '0';
    pulse;
    assert dout = '1' severity failure;

    enable <= '1';
    pulse;
    assert dout = '0' severity failure;

    wait;
  end process;
end behav;
