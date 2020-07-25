entity tb_test is
end tb_test;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_test is
  signal resb      : std_logic;
  signal clk_FF    : std_logic;
  signal ADD_FF    : unsigned(1 downto 0);
  signal CONFIG    : std_logic;
  signal D_FF      : std_logic;
  signal WE        : std_logic;
  signal EN_signal : std_logic;
  signal a : std_logic;
  signal b : std_logic;
  signal c : std_logic;
  signal z : std_logic;
begin
  dut: entity work.test
    port map (
      resb      => resb,
      clk_FF    => clk_FF,
      ADD_FF    => ADD_FF,
      CONFIG    => CONFIG,
      D_FF      => D_FF,
      WE        => WE,
      EN_signal => EN_signal);

  process
    procedure pulse is
    begin
      clk_ff <= '0';
      wait for 1 ns;
      clk_ff <= '1';
      wait for 1 ns;
    end pulse;
  begin
    resb <= '0';
    config <= '1';
    pulse;

    assert en_signal = '0' severity failure;

    resb <= '1';
    d_ff <= '1';
    add_ff <= "00";
    we <= '1';
    pulse;

    assert en_signal = '1' severity failure;

    d_ff <= '0';
    add_ff <= "01";
    pulse;

    assert en_signal = '1' severity failure;

    we <= '0';
    add_ff <= "00";
    pulse;

    assert en_signal = '1' severity failure;

    we <= '1';
    pulse;

    assert en_signal = '0' severity failure;

    wait;
  end process;
end behav;
