entity tb_ent1 is
end tb_ent1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent1 is
  signal clk : std_logic;
  signal dout : std_logic_vector (7 downto 0);
begin
  dut: entity work.ent1
    port map (clk => clk, o => dout);

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
    assert dout = x"10" severity failure;

    pulse;
    assert dout = x"11" severity failure;

    pulse;
    assert dout = x"12" severity failure;

    pulse;
    assert dout = x"13" severity failure;

    pulse;
    assert dout = x"14" severity failure;

    pulse;
    assert dout = x"15" severity failure;

    pulse;
    assert dout = x"16" severity failure;

    pulse;
    assert dout = x"17" severity failure;

    pulse;
    assert dout = x"00" severity failure;

    pulse;
    assert dout = x"00" severity failure;

    wait;
  end process;
end behav;
