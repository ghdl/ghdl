entity tb_cnt01 is
end tb_cnt01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_cnt01 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal clr : std_logic;
  signal en : std_logic;
  signal cnt : std_logic_vector (9 downto 0);
begin
  dut: entity work.cnt01
    port map (clock => clk, reset => rst, clear_count => clr,
              enable => en, counter_out => cnt);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    clk <= '0';
    clr <= '0';
    en <= '0';
    rst <= '1';
    wait for 1 ns;
    assert cnt = "0000000000" severity failure;

    rst <= '0';
    pulse;
    assert cnt = "0000000000" severity failure;

    en <= '1';
    pulse;
    assert cnt = "0000000001" severity failure;

    en <= '0';
    pulse;
    assert cnt = "0000000001" severity failure;

    en <= '1';
    pulse;
    assert cnt = "0000000010" severity failure;

    en <= '1';
    pulse;
    assert cnt = "0000000011" severity failure;

    en <= '1';
    clr <= '1';
    pulse;
    assert cnt = "0000000000" severity failure;

    en <= '1';
    clr <= '0';
    pulse;
    assert cnt = "0000000001" severity failure;

    wait;
  end process;
end behav;
