entity tb_repro3 is
end tb_repro3;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_repro3 is
  subtype lruent is std_logic_vector(4 downto 0);

  signal clk    : std_logic;
  signal oent   : lruent;
  signal hway   : unsigned(1 downto 0);
  signal result : lruent;
begin
  dut: entity work.repro3
    port map (
      clk    => clk,
      oent   => oent,
      hway   => hway,
      result => result);
  
  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 10 ns;
      clk <= '1';
      wait for 10 ns;
    end pulse;
  begin
    oent <= "00000";
    hway <= "00";
    pulse;
    assert result = "10100" severity failure;

    hway <= "11";
    pulse;
    assert result = "00111" severity failure;

    oent <= "00100";
    hway <= "01";
    pulse;
    assert result = "00101" severity failure;

    wait;
  end process;
end behav;
