entity tb_var05 is
end tb_var05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var05 is
  signal clk : std_logic;
  signal sel : std_logic;
  signal a, b : std_logic_vector (1 downto 0);
  signal res : std_logic_vector (1 downto 0);
begin
  dut: entity work.var05
    port map (
      sel => sel,
      a => a,
      b => b,
      res => res);

  process
  begin
    sel <= '1';
    a <= "00";
    b <= "11";
    wait for 1 ns;
    assert res = "11" severity failure;

    sel <= '0';
    a <= "00";
    b <= "11";
    wait for 1 ns;
    assert res = "00" severity failure;

    sel <= '0';
    a <= "10";
    b <= "01";
    wait for 1 ns;
    assert res = "10" severity failure;

    sel <= '1';
    a <= "10";
    b <= "01";
    wait for 1 ns;
    assert res = "01" severity failure;

    wait;
  end process;
end behav;
