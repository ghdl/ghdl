entity tb_var01 is
end tb_var01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var01 is
  signal clk : std_logic;
  signal sel : std_logic_vector (3 downto 0);
  signal res : std_logic_vector (7 downto 0);
begin
  dut: entity work.var01
    port map (
      clk => clk,
      sel => sel,
      res => res);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    sel <= x"5";
    pulse;
    assert res = x"00" report "res=" & to_hstring (res) severity failure;
    
    sel <= x"4";
    pulse;
    assert res = x"85" severity failure;

    sel <= x"6";
    pulse;
    assert res = x"ef" severity failure;

    sel <= x"2";
    pulse;
    assert res = x"85" severity failure;

    wait;
  end process;
end behav;
